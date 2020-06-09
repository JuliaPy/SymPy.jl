##################################################

## promote up to symbolic so that math ops work
promote_rule(::Type{T}, ::Type{S})  where {T<:SymbolicObject, S<:Number}= T
Base.promote_type(::Type{Irrational{T}}, ::Type{Sym}) where {T} = Sym
Base.promote_op(::T, ::Type{S}, ::Type{Sym}) where {T, S <: Number} = Sym
Base.promote_op(::T, ::Type{Sym}, ::Type{S}) where {T, S <: Number} = Sym
Base.promote_op(::T, ::Type{Sym}, ::Type{Sym}) where {T} = Sym # This helps out linear algebra conversions

## Conversion
convert(::Type{T}, o::PyCall.PyObject) where {T <: SymbolicObject} = T(o)
convert(::Type{PyObject}, s::Sym) = s.__pyobject__

## rational
convert(::Type{T}, x::Rational) where {T<:SymbolicObject} = sympy.Rational(x.num, x.den)::T

## big. Need mpmath installed separately -- not as a SymPy module as that is how it is called in PyCall
convert(::Type{T}, x::BigFloat) where {T<:SymbolicObject} = Sym(PyCall.PyObject(x))::T
convert(::Type{Sym}, x::Complex{BigFloat}) = Sym(PyCall.PyObject(x))::Sym

## real
convert(::Type{S}, x::T) where {S<:SymbolicObject, T <: Real}= sympify(x)::S
convert(::Type{T}, x::Sym) where {T <: Real} = convert(T, PyObject(x))


## complex
## cf math.jl for `complex` of a value
## IM is SymPy's "i" (synpy.I, not Python's
## Sym(PyCall.PyObject(im)) which gives 1j.
function convert(::Type{Sym}, x::Complex)
    y = ifelse(isa(x, Complex{Bool}), real(x) + imag(x) * im, x)
    real(y) + imag(y) * IM
end
convert(::Type{Complex{T}}, x::Sym) where {T} = complex(map(x -> convert(T, x), x.as_real_imag())...)


## Irrationals
Base.convert(::Type{Sym}, x::Irrational{:π}) = PI
Base.convert(::Type{Sym}, x::Irrational{:ℯ}) = sympy.exp(1)
Base.convert(::Type{Sym}, x::Irrational{:γ}) = Sym(sympy.EulerGamma)
Base.convert(::Type{Sym}, x::Irrational{:catalan}) = Sym(sympy.Catalan)
Base.convert(::Type{Sym}, x::Irrational{:φ}) = (1 + Sym(5)^(1//2))/2

## utility functions to test type of value
function is_integer(x::Sym)

    pycall_hasproperty(x, :is_integer) && return x.is_integer
    x.__class__.__name__ == "int" && return true

    return false
end

function is_rational(x::Sym)
    pycall_hasproperty(x, :is_rational) && return x.is_rational

    return false
end

function is_real(x::Sym)
    pycall_hasproperty(x, :is_real) && return x.is_real
    x.__class__.__name__ == "real" && return true
    x.__class__.__name__ == "mpg" && return true

    return false
end

function is_complex(x::Sym)
    pycall_hasproperty(x, :is_complex) && return x.is_complex
    x.__class__.__name__ == "complex" && return true
    x.__class__.__name__ == "mpc" && return true

    return false
end

"""
    N(ex)

Convert a `Sym` value to a numeric Julian value.

In SymPy, `N(ex, options...)` is identifcal to `ex.evalf(options...)`
and is used to convert expressions into floating-point
approximations. A positional precision argument indicates the number
of digits, keyword arguments `chop` can be used to trim floating point
roundoff errors and `subs` for free variable substitution prior to
conversions.

For example, symbolic roots can be computed numerically, even if not
available symbolically, by calling `N` on the values.

Using `SymPy` within `Julia` makes having two such functions useful:

* one to do the equivalent of SymPy's `evalf` call
* one to convert these expressions back into `Julia` objects (like `convert(T,  ex)`)

We use `N` to return a `Julia` object and `evalf` to return a symbolic
object. The type of `Julia` object is heurisitically identified.

Examples:

```jldoctest
julia> using SymPy

julia> x = Sym("x")
x

julia> p = subs(x, x, pi)
π

julia> N(p)                            # float version of pi
π = 3.1415926535897...

julia> p.evalf(60)                     # 60 digits of pi, as a symbolic value
3.14159265358979323846264338327950288419716939937510582097494

julia> N(p, 60)                        # when a precision is given, "Big" values are returned
3.141592653589793238462643383279502884197169399375105820974939

julia> r = subs(x,x,1.2)
1.20000000000000

julia> N(r)                            # float
1.2

julia> q = subs(x, x, 1//2)
1/2

julia> N(q)                            # 1//2
1//2

julia> z = solve(x^2 + 1)[1]           # -ⅈ
-ⅈ

julia> N(z)                            # 0 - 1im
0 - 1im

julia> z.evalf()
-1.0⋅ⅈ

julia> rts = solve(x^5 - x + 1)
5-element Array{Sym,1}:
 CRootOf(x^5 - x + 1, 0)
 CRootOf(x^5 - x + 1, 1)
 CRootOf(x^5 - x + 1, 2)
 CRootOf(x^5 - x + 1, 3)
 CRootOf(x^5 - x + 1, 4)

julia> [r.evalf() for r in rts]          # numeric solutions to quintic
5-element Array{Sym,1}:
                       -1.16730397826142
 -0.181232444469875 - 1.08395410131771*I
 -0.181232444469875 + 1.08395410131771*I
 0.764884433600585 - 0.352471546031726*I
 0.764884433600585 + 0.352471546031726*I

julia> [N(r) for r in rts]             
5-element Array{Number,1}:
                     -1.167303978261418684256045899854842180720560371525489039140082449275651903429536
 -0.18123244446987538 - 1.0839541013177107im
 -0.18123244446987538 + 1.0839541013177107im
   0.7648844336005847 - 0.35247154603172626im
   0.7648844336005847 + 0.35247154603172626im
```


`N` returns the value unchanged when it has free symbols.
"""
function N(x::Sym)

    length(free_symbols(x)) > 0 && return x
    # many different possible types, and not all have some nice property
    # python int
    # mpmath int
    # SymPy big int
    # python float
    # Pi, Half, Rational


    for  (u,v) in sympy_core_numbers
        if pycall_hasproperty(x, :__class__)
            if x.__class__.__name__ == string(u)
                return v
            end
        end
    end
    if is_(:real, x)
        if is_(:zero, x)
            return 0
        elseif is_(:infinite, x)
            return (x.is_negative ? -1 : 1) * Inf
        elseif is_(:integer, x)
            u = abs(x)
            if sympy.Le(u, typemax(Int)) == Sym(true)
                return convert(Int, x)
#            elseif sympy.Le(u, typemax(Int128)) == Sym(true)
#                # no PyCall support for this conversion?
#                return convert(Int128, x)
            else
                return convert(BigInt, x)
            end
        elseif x.__class__.__name__ == "Float"
            if x._prec <= 64
                return convert(Float64, x)
            else
                return convert(BigFloat, x)
            end
        elseif is_(:rational, x)
            return N(numer(x)) // N(denom(x))
        elseif pycall_hasproperty(x, :args) && length(x.args) > 1
            def_precision_decimal = ceil(Int, log10(big"2"^Base.MPFR.DEFAULT_PRECISION.x))
            convert(BigFloat, x.evalf(def_precision_decimal))
        elseif length(x.args) > 0
            return N(x.evalf())
        else
            return convert(BigFloat, x)
        end
    elseif x.__class__.__name__ == "ComplexRootOf"
        u = x.evalf(16)
        return N(u)
    elseif is_(:complex, x)
        return complex(N(sympy.re(x)), N(sympy.im(x)))
    elseif is_(:imaginary, x)
        return complex(0, N(sympy.im(x)))
    elseif x.__class__.__name__ == "int"
        convert(Int, x)
    elseif x.__class__.__name__ == "float"
        convert(Float64, x)
    elseif x.__class__.__name__ == "complex"
        return complex(N(sympy.re(x)), N(sympy.im(x)))
    elseif x.__class__.__name__ == "mpf"
        convert(Float64, x)
    elseif x.__class__.__name__ == "mpc"
        return complex(N(sympy.re(x)), N(sympy.im(x)))
    elseif x.__class__.__name__ == "Infinity"
        return Inf
    elseif x.__class__.__name__ == "NegativeInfinity"
        return -Inf
    elseif x.__class__.__name__ == "ComplexInfinity"
        return complex(Inf)
    elseif Eq(x,Sym(true)) == Sym(true)
        return true
    elseif Eq(x, Sym(false)) == Sym(true)
        return false
    else
        @info "FAILED to find type for $x. Please report"
        x
    end
end
N(x::Number) = x  # implies N(x::Sym) = x if ...
N(m::AbstractArray{Sym}) = map(N, m)

# special case numbers in sympy.core.numbers
sympy_core_numbers = ((:Zero, 0),
                      (:One, 1),
                      (:NegativeOne, -1),
                      (:Half, 1//2),
                      (:NaN, NaN),
                      (:Exp1, ℯ),
                      (:ImaginaryUnit, im),
                      (:Pi, pi),
                      (:EulerGamma, Base.MathConstants.eulergamma),
                      (:Catalan, Base.MathConstants.catalan),
                      (:GoldenRation, Base.MathConstants.golden),
                      (:TribonacciConstant, big(1)/3 + (-big(3)*sqrt(big(33)) + 19)^(1//3)/3 + (3*sqrt(big(33)) + 19)^(1//3)/3))


# fix me XXX
"""
    N(x::Sym, digits::Int)

`N` can take a precision argument, whichm when given as an integer greater than 16, we try to match the digits of accuracy using `BigFloat` precision on conversions to floating point.

"""
function N(x::Sym, digits::Int; kwargs...)


    ## check
    digits <= 16 && return(N(x))
    if is_integer(x) == nothing
        out = x.evalf(digits; kwargs...)
        return( N(out, digits) )
    end

    ex = x.evalf(digits)
    if is_integer(x)
        return(convert(BigInt, x))
    elseif is_rational(x)
        return N(numer(x)) / N(denom(x))
    elseif is_real(x) == true
        p = round(Int,log2(10)*digits)

        out = setprecision(p) do
            convert(BigFloat, ex)
        end
        return(out)
    elseif is_complex(x) == true
        r, i = ex.as_real_imag()
        u, v = promote(N(r, digits), N(i, digits))
        return(Complex(u, v))
    end

    N(x.evalf(digits; kwargs...))
    #throw(DomainError())
end






##################################################
##
## infix logical operators

## XXX Experimental! Not sure these are such a good idea ...
## but used with piecewise
import Base: &, |, !
Base.:&(x::Sym, y::Sym) = PyCall.pycall(PyObject(x).__and__, Sym, y)
Base.:|(x::Sym, y::Sym) =  PyCall.pycall(PyObject(x).__or__, Sym, y)
!(x::Sym)         =       PyCall.pycall(PyObject(x).__invert__, Sym)::Sym

## use ∨, ∧, ¬ for |,&,! (\vee<tab>, \wedge<tab>, \neg<tab>)
∨(x::Sym, y::Sym) = x | y
∧(x::Sym, y::Sym) = x & y
¬(x::Sym) = !x
export ∨, ∧, ¬


## In SymPy, symbolic equations are not represented by `=` or `==`
## rather ther function `Eq` is used. Here we use the unicode
## `\Equal<tab>` for an infix operator. There are also unicode values to represent analogs of `<`, `<=`, `>=`. `>`. These are

## * `<`  is `\ll<tab>`
## * `<=` is `\leqq<tab>
## * `==` is `\Equal<tab>`
## * `>=` is `\geqq<tab>`
## * `>`  is `\gg<tab>`


## Instead we have:
## We use unicode for visual appeal of infix operators, but the Lt, Le, Eq, Ge, Gt are the proper way:

"This is `\\ll<tab>` mapped as an infix operator to `Lt`"
(≪)(a::Sym, b::Sym) = Lt(a,b)  # \ll<tab>
(≪)(a::Sym, b::Number) = Lt(a,Sym(b)) # \ll<tab>
(≪)(a::Number, b::Sym) = Lt(Sym(a),b) # \ll<tab>

## could just do this, but it would interfere with other uses outside of SymPy
## (≪)(a::Number, b::Number) = Lt(promote(a,b)...)  # \ll<tab>

"This is `\\leqq<tab>` mapped as an infix operator to `Le`"
(≦)(a::Sym, b::Sym) = Le(a,b)   # \ll<tab>
(≦)(a::Sym, b::Number) = Le(a,Sym(b))  # \ll<tab>
(≦)(a::Number, b::Sym) = Le(Sym(a),b)   # \ll<tab>

"This is `\\gg<tab>` mapped as an infix operator to `Gt`"
(≫)(a::Sym, b::Sym) = Gt(a,b) |> asBool
(≫)(a::Sym, b::Number) = Gt(a,Sym(b))
(≫)(a::Number, b::Sym) = Gt(Sym(a),b)

"This is `\\geqq<tab>` mapped as an infix operator to `Ge`"
(≧)(a::Sym, b::Sym) = Ge(a,b) |> asBool
(≧)(a::Sym, b::Number) = Ge(a,Sym(b))
(≧)(a::Number, b::Sym) = Ge(Sym(a),b)

"For infix `Eq` one can use \\Equal<tab> unicode operator"
(⩵)(a::Sym, b::Sym) = Eq(a,b)  # \Equal<tab>
(⩵)(a::Sym, b::Number) = Eq(a,Sym(b))  # \Equal<tab>
(⩵)(a::Number, b::Sym) = Eq(Sym(a),b)  # \Equal<tab>


export ≪,≦,⩵,≧,≫
