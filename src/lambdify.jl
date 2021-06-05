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

## Mapping of Julia function names into julia ones
## most are handled by Symbol(fnname), this catches exceptions
_heaviside(x) = 1//2 * (1 + sign(x))
function _piecewise(args...)
    as = copy([args...])
    val, cond = pop!(as)
    ex = Expr(:call, :ifelse, cond, val, :nothing)
    while length(as) > 0
        val, cond = pop!(as)
        ex = Expr(:call, :ifelse, cond, val, ex)
    end
    ex
end

## Hack to avoid Expr(:call,  :*,2, x)  being  2x and  not  2*x
## As of newer sympy versions, this is no longer needed.
__prod__(args...) =  prod(args)
export __prod__

## Hide these away. Anonymous function definition is problematic
__SYMPY__ANY__(xs...) = any(xs)
__SYMPY__ALL__(xs...) = all(xs)
__SYMPY__ZERO__(xs...) = 0
__SYMPY__HEAVISIDE__ = (a...)  -> (a[1] < 0 ? 0 : (a[1] > 0 ? 1 : (length(a) > 1 ? a[2] : NaN)))
export __SYMPY__ANY__, __SYMPY__ALL__, __SYMPY__ZERO__, __SYMPY__HEAVISIDE__

fn_map = Dict(
              "Add" => :+,
              "Sub" => :-,
              "Mul" => :*, # was ((as...)->prod(as)), was:__prod__, but :* can now be used
              "Div" => :/,
              "Pow" => :^,
              "re"  => :real,
              "im"  => :imag,
              "Abs" => :abs,
              "Min" => :min,
              "Max" => :max,
              "Poly" => :identity,
              "Piecewise" => :(_piecewise),
              "Order" => :__SYMPY__ZERO__, # :(as...) -> 0,
              "And" => :__SYMPY__ALL__, #:((as...) -> all(as)), #:(&),
              "Or" =>  :__SYMPY__ANY__, #:((as...) -> any(as)), #:(|),
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
    # not quite a match; NaN not θ(0) when evaluated at 0 w/o second argument
    "Heaviside" => :__SYMPY__HEAVISIDE__
              )

map_fn(key, fn_map) = haskey(fn_map, key) ? fn_map[key] : Symbol(key)

Base.convert(::Type{Expr}, x::SymbolicObject) = walk_expression(x)

function walk_expression(ex; values=Dict(), fns=Dict())

    fns_map = merge(fn_map, fns)
    vals_map = merge(val_map, values)

    fn = Introspection.funcname(ex)

    # special case `F(t) = ...` output from ODE
    # this may be removed if it proves a bad idea....
    if fn == "Equality" && lhs(ex).is_Function
        return walk_expression(rhs(ex), values=values, fns=fns)
    end

    if fn == "Symbol" || fn == "Dummy"
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

* `vars` a container of symbols to use for the function arguments. The default is `free_symbols` which has a specific ordering. Specifying `vars` allows this default ordering of arguments to be customized.

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
1.2086779438644711e-15

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

    body = convert_expr(ex, fns=fns, values=values, use_julia_code=use_julia_code)
    body = Meta.parse(string(body)) ## this seems stupid!
    ex = expr_to_function(body, vars)
    if invoke_latest
        fn = eval(ex)
        return (args...) -> Base.invokelatest(fn, args...)
    else
        ex
    end
end

# convert symbolic expression to julia AST
# more flexibly than `convert(Exprt, ex)`
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
