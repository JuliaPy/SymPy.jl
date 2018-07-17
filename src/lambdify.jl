## Lambidfy an expression
## https://github.com/jverzani/SymPy.jl/issues/60
## Hack until SymPy -> Julia converter written

## some tools, perhaps. Not exported for now.
funcname(x) = PyObject(x)[:func][:__name__]
srepr(x) = sympy_meth(:srepr, x)

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
              "Equal" => :(==),
              "GreaterThan" => :(>=),
              "Greater" => :(>) 
              )
              
map_fn(key, fn_map) = haskey(fn_map, key) ? fn_map[key] : Symbol(key)
              
              
## TODO? In SymPy, one can pass in dictionary keys to replace functions
## http://docs.sympy.org/dev/tutorial/manipulation.html
function walk_expression(ex; values=Dict(), fns=Dict())

    fns_map = merge(fn_map, fns)
    vals_map = merge(val_map, values)
    
    fn = funcname(ex)
    
    if fn == "Symbol"
        return Symbol(string(ex))
    elseif fn in ["Integer" , "Float"]
        return N(ex)
    elseif fn == "Rational"
        return convert(Int, numer(ex))//convert(Int, denom(ex))
        ## piecewise requires special treatment
    elseif fn == "Piecewise"
        return _piecewise([walk_expression(cond) for cond in args(ex)]...)
    elseif fn == "ExprCondPair"
        val, cond = args(ex)
        return (val, walk_expression(cond))
    elseif haskey(vals_map, fn)
        return vals_map[fn]
    end

    as = args(ex)

    Expr(:call, map_fn(fn, fns_map), [walk_expression(a) for a in as]...)
end

"""

`lambidfy(ex::Sym,[vars]; typ=Any)`: Lambidfy an expression returning a
native Julia function.  SymPy's
[lambdify](http://docs.sympy.org/dev/modules/utilities/lambdify.html)
function translates code into Python, this translates an expression
into a `Julia` function.

Evaluating the function does not call into SymPy, so should be much faster.

The optional `[vars]` specifies the order of the variables when more
than one is in `ex`. The default is to use the ordering of
`free_symbols(ex)`.

The keyword aruguments allow for the passing of expressions that are
not covered by the default ones. These are dictionaries whose keys are
strings with a SymPy name and whose values are symbols representing
`Julia` values. For examples `Dict("sin"=>:sin)` could be used to map
a function, were that not already done.

Additionally, a DataType keyword can be specified for the function.

Not all expressions can be lambdified. If not, an error is thrown.

Some simple examples

```
@vars x y
lambdify(x^2)(2)       # 4
lambdify(x*y^2)(2,3)   # 2*3^2 using default ordering
lambdify(x*y^2, [y, x])(2,3) # 3*2^2, as function is (y,x) -> x*y^2 equivalent in Julia
```


Compare times
```
xs = rand(1000)
@vars x
ex = sin(x)*cos(2x) * exp(x^2/2)
map(u -> N(ex(u)), xs)   # 3.435850 seconds
map(lambdify(ex), xs)    # 0.007085 seconds
```

This is a *temporary* solution. The proper fix is to do this in SymPy.

"""
function lambdify(ex::Sym, vars=free_symbols(ex); typ=Any, fns=Dict(), values=Dict())
    # if :julia_code printer is there, use it
    # if haskey(sympy, :julia_code)
    #     body = parse(sympy_meth(:julia_code, ex)) # issue here with 2.*...
    # else
    #     body = walk_expression(ex, fns=fns, values=values)
    # end
    body = walk_expression(ex, fns=fns, values=values)    
    try
        syms = typ == Any ? map(Symbol,vars) : map(s->Expr(:(::),s,typ), Symbol.(vars))
        fn = eval(Expr(:function, Expr(:call, gensym(), syms...), body))
        (args...) -> Base.invokelatest(fn, args...) # https://github.com/JuliaLang/julia/pull/19784
    catch err
        throw(ArgumentError("Expression does not lambdify"))
    end
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

"""

`lambidfy_expr(ex::Sym,[vars]; typ=Any, name=gensym())`: Lambidfy an expression
returning a native Julia function `Expr` object.  SymPy's
[lambdify](http://docs.sympy.org/dev/modules/utilities/lambdify.html)
function translates code into Python, this translates an expression
into a `Julia` function `Expr` object.

The function can be evaluated and doesn't call SymPy, so should be much faster.

The optional `[vars]` specifies the order of the variables when more
than one is in `ex`. The default is to use the ordering of
`free_symbols(ex)`.

The keyword aruguments allow for the passing of expressions that are
not covered by the default ones. These are dictionaries whose keys are
strings with a SymPy name and whose values are symbols representing
`Julia` values. For examples `Dict("sin"=>:sin)` could be used to map
a function, were that not already done.

Additionally, a DataType keyword can be specified for the function.

Not all expressions can be lambdified. If not, an error is thrown.

Some simple examples

```
@vars x y
lambdify_expr(x^2;typ=Int,name=:square)
lambdify_expr(x*y^2)            # using default ordering
lambdify_expr(x*y^2, [y, x])    # alternate ordering
```

"""
function lambdify_expr(ex::Sym, vars=free_symbols(ex); name=gensym(), typ=Any, fns=Dict(), values=Dict())
    # if :julia_code printer is there, use it
    # if haskey(sympy, :julia_code)
    #     body = parse(sympy_meth(:julia_code, ex))
    # else
    #     body = walk_expression(ex, fns=fns, values=values)
    # end
    body = walk_expression(ex, fns=fns, values=values)    
    try
        syms = typ == Any ? map(Symbol,vars) : map(s->Expr(:(::),s,typ), Symbol.(vars))
        Expr(:function, Expr(:call, name, syms...), body)
    catch err
        throw(ArgumentError("Expression does not lambdify"))
    end
end

export(lambdify_expr)

function init_lambdify()
#    if haskey(sympy, :julia_code)
#        global julia_code(ex::Sym; assign_to=nothing, kwargs...) = sympy_meth(:julia_code, ex; assign_to=assign_to, kwargs...)
#        julia_code(ex::Sym; assign_to=nothing, kwargs...) = sympy_meth(:julia_code, ex; assign_to=assign_to, kwargs...)
#        eval(Expr(:export, :julia_code))
#    end
end

