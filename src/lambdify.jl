# lambdify an expression



## Mapping of SymPy Values into julia values
val_map = Dict(
               "Zero"             => :(0),
               "One"              => :(1),
               "NegativeOne"      => :(-1),
               "Half"             => :(1/2),
               "Pi"               => :pi,
               "Exp1"             => :e,
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


fn_map = Dict(
              "Add" => :+,
              "Sub" => :-,
              "Mul" => :*,
              "Div" => :/,
              "Pow" => :^,
              "re"  => :real,
              "im"  => :imag,
              "Abs" => :abs,
              "Min" => :min,
              "Max" => :max,
              "Poly" => :identity,
              "Heaviside" => :(_heaviside),
              "Piecewise" => :(_piecewise),
              "And" => :(&),
              "Or" => :(|),
              "Less" => :(<),
              "LessThan" => :(<=),
              "StrictLessThan" => :(<),
              "Equal" => :(==),
              "StrictGreaterThan" => :(>),
              "GreaterThan" => :(>=),
"Greater" => :(>),
"conjugate" => :conj
              )

map_fn(key, fn_map) = haskey(fn_map, key) ? fn_map[key] : Symbol(key)

Base.convert(::Type{Expr}, x::SymbolicObject) = walk_expression(x)

function walk_expression(ex; values=Dict(), fns=Dict())

    fns_map = merge(fn_map, fns)
    vals_map = merge(val_map, values)

    fn = Introspection.funcname(ex)

    if fn == "Symbol"
        return Symbol(string(ex))
    elseif fn in ["Integer" , "Float"]
        return N(ex)
    elseif fn == "Rational"
        return convert(Int, numer(ex))//convert(Int, denom(ex))
        ## piecewise requires special treatment
    elseif fn == "Piecewise"
        return _piecewise([walk_expression(cond) for cond in Introspection.args(ex)]...)
    elseif fn == "ExprCondPair"
        val, cond = Introspection.args(ex)
        return (val, walk_expression(cond))
    elseif haskey(vals_map, fn)
        return vals_map[fn]
    end

    as = Introspection.args(ex)

    Expr(:call, map_fn(fn, fns_map), [walk_expression(a) for a in as]...)
end

"""
     lambdify(ex, vars; typ, fns, values, use_julia_code, invoke_latest)

Take a symbolic expression and return an anonymous `Julia` function

Converts from a SymPy object to an expression by walking the SymPy expression tree and converting each step. Then creates a function. The function arguments are based on `free_symbols`, and its ordering unless `vars` is specified directly.

* `use_julia_code=false` will use SymPy's conversion to an expression, the default is `false`

* `invoke_latest=true` calls `Base.invokelatest` to work around world age issues. This is th safe default, but setting to `false` will result in faster-executing functions.

Example:

```
@vars x y z
ex = x^2 * sin(x)
fn = lambdify(ex)
fn(pi)

ex = x + 2y + 3z
fn = lambdify(ex)
fn(1,2,3) # order is y,x,z

fn = lambdify(ex, (x,y,z))
fn(1,2,3)
```

!!! note

    Ideally, this would just be:

```
body = Meta.parse(julia_code(ex))
syms = Symbol.(free_symbols(ex))
fn = eval(Expr(:function, Expr(:call, gensym(), syms...), body))
```

Where the first line could also be `convert(Expr, ex)`. However, the `julia_code` method from sympy needs some attention.

"""
function lambdify(ex::Sym, vars=free_symbols(ex);
                  typ=Any, fns=Dict(), values=Dict(),
                  use_julia_code=false,
                  invoke_latest=true
                  )
    # if :julia_code printer is there, use it
    if use_julia_code
        body = Meta.parse(sympy.julia_code(ex)) # issue here with 2.*...
    else
        body = walk_expression(ex, fns=fns, values=values)
    end

    try
        syms = typ == Any ? map(Symbol,vars) : map(s->Expr(:(::),s,typ), Symbol.(vars))
        fn = eval(Expr(:function, Expr(:call, gensym(), syms...), body))
        if invoke_latest
            (args...) -> Base.invokelatest(fn, args...) # https://github.com/JuliaLang/julia/pull/19784
        else
            fn
        end
    catch err
        throw(ArgumentError("Expression does not lambdify"))
    end
end


function _lambdify(ex::Sym, vars=free_symbols(ex))
    body = convert(Expr, ex)
    syms = Symbol.(vars)
    fn = eval(Expr(:function, Expr(:call, gensym(), syms...), body))
    fn
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

export(lambdify)

Base.convert(::Type{Function}, ex::Sym) = lambdify(ex)
