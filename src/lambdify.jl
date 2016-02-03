## Lambidfy an expression
## https://github.com/jverzani/SymPy.jl/issues/60
## Hack until SymPy -> Julia converter written
_func(x) = (x.x)[:func]
_funcname(x) = _func(x)[:__name__]
_args(x) = (x.x)[:args]
_srepr(x) = sympy_meth(:srepr, x)

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
               "ImaginaryUnit"    => :im
               )

## Mapping of Julia function names into julia ones
## most are handled by symbol(fnname), this catches exceptions
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
              "Max" => :max
              )
              
map_fn(key) = haskey(fn_map, key) ? fn_map[key] : symbol(key)              
              
              
## TODO? In SymPy, one can pass in dictionary keys to replace functions
## http://docs.sympy.org/dev/tutorial/manipulation.html
function walk_expression(ex)

    fn = _funcname(ex)
    
    if fn == "Symbol"
        return symbol(string(ex))
    elseif fn in ["Integer" , "Float"]
        return N(ex)
    elseif fn == "Rational"
        return convert(Int, numer(ex))//convert(Int, denom(ex))        
    elseif haskey(val_map, fn)
        return val_map[fn]
    end

    as = _args(ex)

    Expr(:call, map_fn(fn), [walk_expression(a) for a in as]...)
end

"""

`lambidfy(ex::Sym,[vars])`: Lambidfy an expression returning a native
Julia function.  SymPy's
[lambdify](http://docs.sympy.org/dev/modules/utilities/lambdify.html)
function translates code into Python, this translates an expression
into a `Julia` function.

Evaluating the function does not call into SymPy, so should be much faster.

The optional `[vars]` specifies the order of the variables when more
than one is in `ex`. The default is to use the ordering of
`free_symbols(ex)`.

Not all expressions can be lambdified. If not, an error is thrown.

Some simple examples

```
@vars x y
lambdify(x^2)(2)       # 4
lambdify(x*y^2)(2,3)   # 2*3^2 using default ordering
lambdify(x*y^2, [y, x])(2,3) # 3*2^2, as function is (y,x) -> x*y^2 equivalent in Julia



Compare times
```
xs = rand(1000)
@vars x
ex = sin(x)*cos(2x) * exp(x^2/2)
map(u -> N(ex(u)), xs)   # 3.435850 seconds
SymPy.mapsubs(ex, x, xs) # 0.008569 seconds (does calculuations in Python)
map(lambdify(ex), xs)    # 0.007085 seconds
```

This is a *temporary* solution. The proper fix is to do this in SymPy.

"""
function lambdify(ex::Sym, vars=free_symbols(ex))
    try
        eval(Expr(:function,
                  Expr(:call, gensym(), map(symbol,vars)...),
                  walk_expression(ex)))
    catch err
        throw(ArgumentError("Expression does not lambdify"))
    end
end

export(lambdify)
