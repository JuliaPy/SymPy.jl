# lambdify an expression



## Mapping of SymPy Values into julia values
val_map = Dict(
               "Zero"             => :(0),
               "One"              => :(1),
               "NegativeOne"      => :(-1),
               "Half"             => :(1/2),
               "Pi"               => :pi,
               "Exp1"             => :ℯ,
               "Infinity"         => :Inf,
               "NegativeInfinity" => :(-Inf),
               "ComplexInfinity"  => :Inf, # error?
               "ImaginaryUnit"    => :im,
               "BooleanTrue"      => :true,
               "BooleanFalse"     => :false
               )

function _piecewise(args...)
    as = copy([args...])
    val, cond = pop!(as)
    ex = Expr(:call, :ifelse, cond, convert(Expr,val), :nothing)
    while length(as) > 0
        val, cond = pop!(as)
        ex = Expr(:call, :ifelse, cond, convert(Expr,val), convert(Expr, ex))
    end
    ex
end


## Mapping of Julia function names into julia ones
## most are handled by Symbol(fnname), the following catch exceptions
## Hack to avoid Expr(:call,  :*,2, x)  being  2x and  not  2*x
## As of newer sympy versions, this is no longer needed.
__PROD__(args...) =  prod(args)

__ANY__(xs...) = any(xs)
__ALL__(xs...) = all(xs)
__ZERO__(xs...) = 0
# not quite a match; NaN not θ(0) when evaluated at 0 w/o second argument
__HEAVISIDE__ = (a...)  -> (a[1] < 0 ? 0 : (a[1] > 0 ? 1 : (length(a) > 1 ? a[2] : NaN)))
__POW__(x, y::Int) = Base.literal_pow(^, x, Val(y)) # otherwise
__POW__(a,b) = (@show a, b; (a)^(b))
#  __SYMPY__ALL__,
fn_map = Dict(
    "Add" => :+,
    "Sub" => :-,
    "Mul" => :*, # :(SymPy.__PROD__)
    "Div" => :/,
    "Pow" => :^,
    #"Pow" => :(SymPy.__POW__),
    "re"  => :real,
    "im"  => :imag,
    "Abs" => :abs,
    "Min" => :min,
    "Max" => :max,
    "Poly" => :identity,
    "Piecewise" => :(SymPy._piecewise),
    "Order" => :(SymPy.__ZERO__), # :(as...) -> 0,
    "And" => :(SymPy.__ALL__), #:((as...) -> all(as)), #:(&),
    "Or" =>  :(SymPy.__ANY__), #:((as...) -> any(as)), #:(|),
    "Less" => :(<),
    "LessThan" => :(<=),
    "StrictLessThan" => :(<),
    "Equal" => :(==),
    "Equality" => :(==),
    "Unequality" => :(!==),
    "StrictGreaterThan" => :(>),
    "GreaterThan" => :(>=),
    "Greater" => :(>),
    "conjugate" => :conj,
    "atan2" => :atan,
    "TupleArg" => :tuple,
    "Heaviside" => :(SymPy.__HEAVISIDE__),
)

map_fn(key, fn_map) = haskey(fn_map, key) ? fn_map[key] : Symbol(key)

Base.convert(::Type{Expr}, x::SymbolicObject) = walk_expression(x)

"""
    walk_expression(ex; values=Dict(), fns=Dict())

Convert a symbolic SymPy expression into a `Julia` expression. This is needed to use functions in external packages in lambdified functions.

## Example
```
using SymPy
@syms x y
ex = sympy.hyper((2,2),(3,3),x) * y
```

Calling `lambdify(ex)` will fail to make a valid function, as `hyper` is implemented in `HypergeometricFunctions.pFq`. So, we have:

```
using HypergeometricFunctions
d = Dict("hyper" => :pFq)
body = SymPy.walk_expression(ex, fns=d)
syms = Symbol.(free_symbols(ex))
fn = eval(Expr(:function, Expr(:call, gensym(), syms...), body));
fn(1,1) # 1.6015187080185656
```

"""
function walk_expression(ex; values=Dict(), fns=Dict())

    fns_map = merge(fn_map, fns)
    vals_map = merge(val_map, values)

    fn = Introspection.funcname(ex)

    # special case `F(t) = ...` output from ODE
    # this may be removed if it proves a bad idea....
    if fn == "Equality" && lhs(ex).is_Function
        return walk_expression(rhs(ex), values=values, fns=fns)
    end

    if fn == "Symbol" || fn == "Dummy" || fn == "IndexedBase"
        str_ex = string(ex)
        return get(vals_map, str_ex, Symbol(str_ex))
    elseif fn in ["Integer" , "Float"]
        return N(ex)
    elseif fn == "Rational"
        return convert(Int, numer(ex))//convert(Int, denom(ex))
        ## piecewise requires special treatment
    elseif fn == "Piecewise"
        return _piecewise([walk_expression(cond, values=values, fns=fns) for cond in Introspection.args(ex)]...)
    elseif fn == "ExprCondPair"
        val, cond = Introspection.args(ex)
        return (val, walk_expression(cond, values=values, fns=fns))
    elseif fn == "Tuple"
        return walk_expression.(Introspection.args(ex), values=values, fns=fns)
    elseif fn == "Indexed"
        return Expr(:ref, [walk_expression(a, values=values, fns=fns) for a in Introspection.args(ex)]...)
    elseif fn == "Pow"
        a, b = Introspection.args(ex)
        b == 1//2 && return Expr(:call, :sqrt, walk_expression(a, values=values, fns=fns))
        b == 1//3 && return Expr(:call, :cbrt, walk_expression(a, values=values, fns=fns))
        return Expr(:call, :^,  [walk_expression(aᵢ, values=values, fns=fns) for aᵢ in (a,b)]...)
    elseif haskey(vals_map, fn)
        return vals_map[fn]
    end

    as = Introspection.args(ex)
    Expr(:call, map_fn(fn, fns_map), [walk_expression(a, values=values, fns=fns) for a in as]...)
end

"""
    lambdify(ex, vars=free_symbols();
             fns=Dict(), values=Dict, use_julia_code=false,
             invoke_latest=true)

Take a symbolic expression and return a `Julia` function or expression to build a function.

* `ex::Sym` a symbolic expression with 0, 1, or more free symbols

* `vars` a container of symbols to use for the function arguments. The default is `free_symbols` which has a specific ordering. Specifying `vars` allows this default ordering of arguments to be customized. If `vars` is empty, such as when the symbolic expression has *no* free symbols, a variable arg constant function is returned.

* `fns::Dict`, `vals::Dict`: Dictionaries that allow customization of the function that walks the expression `ex` and creates the corresponding AST for a Julia expression. See `SymPy.fn_map` and `SymPy.val_map` for the default mappings of sympy functions and values into `Julia`'s AST.

* `use_julia_code::Bool`: use SymPy's conversion to an expression, the default is `false`

* `invoke_latest=true`: if `true` will call `eval` and `Base.invokelatest` to return a function that should not have any world age issue. If `false` will return a Julia expression that can be `eval`ed to produce a function.

Example:

```jldoctest
julia> using SymPy

julia> @syms x y z
(x, y, z)

julia> ex = x^2 * sin(x)
 2
x ⋅sin(x)

julia> fn = lambdify(ex);

julia> fn(pi)
0.0

julia> ex = x + 2y + 3z
x + 2⋅y + 3⋅z

julia> fn = lambdify(ex);

julia> fn(1,2,3) # order is by free_symbols
14

julia> ex(x=>1, y=>2, z=>3)
14

julia> fn = lambdify(ex, (y,x,z));

julia> fn(1,2,3)
13
```

!!! Note:

The default produces slower functions due to the calls to `eval` and
`Base.invokelatest`.  In the following `g2` (which, as seen, requires
additional work to compute) is as fast as calling `f` (on non symbolic
types), whereas `g1` is an order of magnitude slower in this example.

```
julia> @vars x
(x,)

julia> f(x) = exp(cot(x))
f (generic function with 1 method)

julia> g1 = lambdify(f(x))
#88 (generic function with 1 method)

julia> ex = lambdify(f(x), invoke_latest=false)
:(function var"##271"(x)
      exp(cot(x))
  end)

julia> @eval g2(x) = (\$ex)(x)
g2 (generic function with 1 method)
```

An alternative, say, is to use `GeneralizedGenerated`'s `mk_function`, as follows:

```
julia> using GeneralizedGenerated

julia> body = convert(Expr, f(x))
:(exp(cot(x)))

julia> g3 = mk_function((:x,), (), body)
function = (x;) -> begin
    (Main).exp((Main).cot(x))
end
```

This function will be about 2-3 times slower than `f`.

"""
function  lambdify(ex::Sym, vars=free_symbols(ex);
              fns=Dict(), values=Dict(),
              use_julia_code=false,
              invoke_latest=true)
    if isempty(vars)
        val′ = convert(Expr, ex)
        val = isa(val′, Symbol) ? getfield(Main, val′) : val′
        return (ts...) -> val
    end
    body = convert_expr(ex, fns=fns, values=values, use_julia_code=use_julia_code)
    ex = expr_to_function(body, vars)
    if invoke_latest
        fn = eval(ex)
        return (args...) -> Base.invokelatest(fn, args...)
    else
        ex
    end
end

# convert symbolic expression to julia AST
# more flexibly than `convert(Expr, ex)`
function convert_expr(ex::Sym;
                      fns=Dict(), values=Dict(),
                      use_julia_code=false)
    if use_julia_code
        body = Meta.parse(sympy.julia_code(ex)) # issue here with 2.*...
    else
        body = walk_expression(ex, fns=fns, values=values)
    end
    body
end

# take an expression and arguments and return an Expr of a generic function
function  expr_to_function(body, vars)
    syms = Symbol.(vars)
    Expr(:function, Expr(:call, gensym(), syms...), body)
end

# from @mistguy cf. https://github.com/JuliaPy/SymPy.jl/issues/218
# T a data type to convert to, when specified
function lambdify(exs::Array{S, N}, vars = union(free_symbols.(exs)...); T::DataType=Nothing, kwargs...) where {S <: Sym, N}
    f = lambdify.(exs, (vars,)) # prevent broadcast in vars
    if T == Nothing
        (args...) -> map.(f, args...)
    else
        (args...) -> convert(Array{T,N}, map.(f, args...))
    end
end

Base.convert(::Type{Function}, ex::Sym) = lambdify(ex)

export lambdify
