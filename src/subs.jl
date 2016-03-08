## Substitutions and numeric conversions


## subs
"""

`subs` is used to subsitute a value in an expression with another
value. The `replace` function is also overrided to do this task.

Examples:

```
x,y = symbols("x,y")
ex = (x-y)*(x+2y)
subs(ex, (y, y^2))
subs(ex, (x,1), (y,2))
subs(ex, (x,y^3), (y,2))
subs(ex, y, 3)
```

There is a curried form of `subs` to use with the chaining `|>` operator

```
ex |> subs(x,e)
```

Since version 0.4, the use of pairs gives a convenient alternative:

```
subs(ex, x=>1, y=>2)
ex |> subs(x=>1, y=>2)
```

There were some older convenience forms, but these will be deprecated, as they don't work as expected when a variable has assumptions. These are:

```
subs(ex, :y, pi)    
subs(ex, x=1, y=pi)
## or
ex |> subs(:x, e)   
ex |> subs(x=e)     
## or as of version 0.4:
ex(x=2, y=3)     
```
"""
subs{T <: SymbolicObject}(ex::T, y::@compat(Tuple{SymbolicTypes, Any})) =
    object_meth(ex, :subs, Sym(y[1]), convert(Sym,y[2]))
subs{T <: SymbolicObject}(ex::T, y::@compat(Tuple{SymbolicTypes, Any}), args...) = subs(subs(ex, y), args...)
subs{T <: SymbolicObject, S<:SymbolicObject}(ex::T, y::S, val) = subs(ex, (y,val))
subs{T <: SymbolicObject}(ex::T, dict::Dict) = subs(ex, dict...)
if VERSION >= v"0.4.0"
    subs{T <: SymbolicObject}(ex::T, d::Pair...) = subs(ex, [(p.first, p.second) for p in d]...)
end
function subs{T <: SymbolicObject, S <: Symbol}(ex::T, y::S, val)
    warn("Calling subs with a symbol and not a symbolic variable is deprecated")
    subs(ex, (y,val))
end

## curried versions to use with |>
subs(x::SymbolicObject, y) = ex -> subs(ex, x, y)
subs(;kwargs...) = ex -> subs(ex; kwargs...)
subs(dict::Dict) = ex -> subs(ex, dict...)
if VERSION >= v"0.4.0"
    subs(d::Pair...) = ex -> subs(ex, [(p.first, p.second) for p in d]...)
end
function subs(x::Symbol, y)
    warn("Calling `subs` with a symbol and not a symbolic variable is deprecated")
    ex -> subs(ex, Sym(x), y)
end

## Convenience method for keyword arguments
## Will deprecate this
function subs{T <: SymbolicObject}(ex::T; kwargs...)
    warn("""
Calling `subs` with keyword arguments will be deprecated. From v0.4 onward, the use of pairs, as in
`subs(ex, var1=>val1, var2=>val2)` is suggested.
""")
    subs(ex, kwargs...)
end

## replace alias for subs
Base.replace(ex::SymbolicObject, x::SymbolicObject, y) = subs(ex, x, y)
## curried version to use through |> as in
## ex |> replace(x, 2)
Base.replace(x::SymbolicObject, y) = ex -> subs(ex, x, y)
Base.replace(ex::SymbolicObject; kwargs...) = subs(ex, kwargs...)


# """
#
# As of version 0.4, expressions are callable. The following works
# using pairs, a dictionary or, one can not specify the variables and
# use the order of the free symbols;

# ```
# ex = (x-3)*(y^2
# ex(x=>1, y=>2)
# ex(Dict(x=>1, y=>2))
# ex(1,2)  ## uses order of free_symbols
# ```
#     """

## add call interface depends on version
VERSION >= v"0.5.0-" && include("call.jl")
v"0.4.0" <= VERSION < v"0.5.0-" && include("call-0.4.jl")


## different conversions


## helper, as :is_rational will find 1.2 rational...
function _is_rational(ex::Sym)
    ex.x[:is_rational] == nothing && return false
    ex.x[:is_rational] && denom(ex).x[:is_integer]
end

## evalf, n, N
## We want to convert to numeric:
## evalf -> keep as sympy expression
## N bring back into julia using convert
## In SymPy N and evalf are the same, so this could be confusing!!!

"""

Convert a `Sym` value to a numeric Julian value.

The `N` function of SymPy is an alias for `evalf`. Within SymPy, either may be used to
find numeric values from symbolic values.

For example, symbolic roots can be computed numerically, even if not
available symbolically, by calling `N` on the values.

Using `SymPy` within `Julia` makes having two such functions useful:

* one to do the equivalent of SymPy's `evalf` function 
* one to *also* convert these expressions back into `Julia` objects.

We use `N` to return a `Julia` object and `evalf` to return a symbolic
object.

Examples:
```
x = Sym("x")
p = subs(x, x, pi)
N(p)                            # float version of pi
evalf(p, 60)                    # 60 digits of pi, as a symbolic value
N(p, 60)                        # when a precision is given, "Big" values are returned
r = subs(x,x,1.2)
N(r)                            # float
q = subs(x, x, 1//2)
N(q)                            # 1//2
z = solve(x^2 + 1)[1]           # -ⅈ
N(z)                            # 0 - 1im
evalf(z)

rts = solve(x^5 - x + 1)
[N(r) for r in rts]             # numeric solutions to quintic
```


The `evalf` function is similar, though it leaves the expression as a symbolic object.
This breaks the similarity of N and evalf for sympy users.

Throws a `DomainError` if no conversion is possible, such as when the expression still has symbolic values.

`N` is type unstable.

"""
function N(ex::Sym)
    ## more work than I'd like
    if ex.x[:is_integer] == nothing
        return(N(ex[:evalf]()))
    end
    if ex.x[:is_integer]
        for T in [Int, BigInt]
            try (return(convert(T, ex))) catch e end
        end
    elseif _is_rational(ex)
        return N(numer(ex)) // N(denom(ex))
        ## `convert(Rational, ex)))` fails on `Sym(4//3)`
    elseif ex.x[:is_real]
        for T in [Irrational, Float64] ## BigFloat???
              try (return(convert(T, ex))) catch e end
        end
    elseif ex.x[:is_complex]
        try
            r, i = ex[:re](), ex[:im]()
            r, i = promote(N(r), N(i))
            return(Complex(r, i))
        catch e
        end
    end
    throw(DomainError())
end
N(x::Number) = x
N(m::Array{Sym}) = map(N, m)
"""
`N` can take a precision argument. 

When given as an integer greater than 16, we try to match the digits of accuracy using `BigFloat` precision on conversions to floating point.

"""
function N(x::Sym, digits::Int)
    ## check
    digits <= 16 && return(N(x))
    if x.x[:is_integer] == nothing
        out = x[:evalf](digits)
        return( N(out, digits) )
    end
    
    ex = evalf(x, digits)
    if x.x[:is_integer]
        return(convert(BigInt, x))
    elseif _is_rational(x)
        return N(numer(x)) / N(denom(x))
    elseif x.x[:is_real]
        p = round(Int,log2(10)*digits)
        
        out = setprecision(p) do 
            convert(BigFloat, ex)
        end
        return(out)
    elseif x.x[:is_complex]
        r, i = ex[:re](), ex[:im]()
        u, v = promote(N(r, digits), N(i, digits))
        return(Complex(u, v))
    end
    
    throw(DomainError())
end

"""

The `evalf` function has keyword possibilities, such as `subs` being given directly through a `Dict(Sym,Any)`.
Unlike `N`, `evalf` returns an object of type `Sym`.

Examples
```
x = Sym("x")
evalf(x, subs=Dict([(x,1/2)])) 
```
"""

function evalf(x::Sym, args...; kwargs...)
    x[:evalf](args...; kwargs...)
end



