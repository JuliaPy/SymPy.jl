## Substitutions and numeric conversions


## subs
"""

`subs` is used to subsitute a value in an expression with another
value. 

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

The use of pairs gives a convenient alternative:

```
subs(ex, x=>1, y=>2)
ex |> subs(x=>1, y=>2)
```

The "`call`" syntax is also employed:

```
ex = x^2 - 2
ex(3)
```

It also works for 2 or more  variables, but the order of which variable is assigned which value is determined by `free_symbols`:

```
@vars x y z
ex = z^y
ex(2,3)  # 2^3 as free_symbols(z^y) is [z,y]
```



There were some older convenience forms, but these are now deprecated, as they don't work as expected when a variable has assumptions. These were:

```
subs(ex, :y, pi)    # using a symbol, not a symbolic object
subs(ex, x=1, y=pi) # using keyword argument, and not pairs
## or their curried or call forms
ex |> subs(:x, e)   
ex |> subs(x=e)     
ex(x=2, y=3)     
```

The `replace` function is related, but not identical to subs.

"""
subs(ex::T, y::Tuple{Any, Any}) where {T <: SymbolicObject}=
    object_meth(ex, :subs, Sym(y[1]), convert(Sym,y[2]))
subs(ex::T, y::Tuple{Any, Any}, args...) where {T <: SymbolicObject} = subs(subs(ex, y), args...)
subs(ex::T, y::S, val) where {T <: SymbolicObject, S<:SymbolicObject} = subs(ex, (y,val))
subs(ex::T, dict::Dict) where {T <: SymbolicObject} = subs(ex, dict...)
subs(ex::T, d::Pair...) where {T <: SymbolicObject} = subs(ex, [(p.first, p.second) for p in d]...)
# matrix interace in `matrix.jl`
subs(exs::Tuple{T, N}, args...;kwargs...) where {T <: SymbolicObject, N} = map(u -> subs(u, args...;kwargs...), exs)
subs(x::Number, args...) = x

## curried versions to use with |>
subs(x::SymbolicObject, y) = ex -> subs(ex, x, y)
subs(;kwargs...) = ex -> subs(ex; kwargs...)
subs(dict::Dict) = ex -> subs(ex, dict...)
subs(d::Pair...) = ex -> subs(ex, [(p.first, p.second) for p in d]...)

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

Returns the value unchanged when it has free symbols.

`N` is type unstable.

"""
function N(ex::Sym)
    ## more work than I'd like
    ## XXX consolidate this and N(ex, digits)

    length(free_symbols(ex)) > 0 && return ex
    
    if is_integer(ex) == nothing
        evalf_ex = ex[:evalf]()
        if ex == evalf_ex
            return ex
        else
            return N(evalf_ex)
        end
    end
    if is_integer(ex)
        for T in [Int, BigInt]
            try (return(convert(T, ex))) catch e end
        end
    elseif _is_rational(ex)
        return N(numer(ex)) // N(denom(ex))
        ## `convert(Rational, ex)))` fails on `Sym(4//3)`
    elseif is_real(ex) == true
        for T in [Irrational, Float64] ## BigFloat???
              try (return(convert(T, ex))) catch e end
        end
    elseif is_complex(ex) == true
        try
            r, i = ex[:re](), ex[:im]()
            r, i = promote(N(r), N(i))
            return(Complex(r, i))
        catch e
        end
    end
    throw(DomainError())
end
N(x::Number) = x  # implies N(x::Sym) = x if ...
N(m::AbstractArray{Sym}) = map(N, m)
"""
`N` can take a precision argument. 

When given as an integer greater than 16, we try to match the digits of accuracy using `BigFloat` precision on conversions to floating point.

"""
function N(x::Sym, digits::Int)
    ## check
    digits <= 16 && return(N(x))
    if is_integer(x) == nothing
        out = x[:evalf](digits)
        return( N(out, digits) )
    end
    
    ex = evalf(x, digits)
    if is_integer(x)
        return(convert(BigInt, x))
    elseif _is_rational(x)
        return N(numer(x)) / N(denom(x))
    elseif is_real(x) == true
        p = round(Int,log2(10)*digits)

        out = setprecision(p) do 
            convert(BigFloat, ex)
        end
        return(out)
    elseif is_complex(x) == true
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
evalf(x::Sym, args...; kwargs...) = object_meth(x, :evalf, args...; kwargs...)



