##################################################

## promote up to symbolic so that math ops work
promote_rule(::Type{T}, ::Type{S})  where {T<:SymbolicObject, S<:Number}= T
Base.promote_type(::Type{Irrational{T}}, ::Type{Sym}) where {T} = Sym
## Conversion
convert(::Type{T}, o::PyCall.PyObject) where {T <: SymbolicObject} = T(o)
convert(::Type{PyObject}, s::Sym) = s.x

## rational
convert(::Type{T}, x::Rational) where {T<:SymbolicObject} = sympy.Rational(x.num, x.den)::T

## big. Need mpmath installed separately -- not as a SymPy module as that is how it is called in PyCall
convert(::Type{T}, x::BigFloat) where {T<:SymbolicObject} = Sym(PyCall.PyObject(x))::T
convert(::Type{Sym}, x::Complex{BigFloat}) = Sym(PyCall.PyObject(x))::Sym

## real
convert(::Type{S}, x::T) where {S<:SymbolicObject, T <: Real}= sympy.sympify(x)::S
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




## ## properties
## number_properties = (:is_even, :is_odd,
##                      :is_number, :is_integer, :is_real,
##                      :is_complex, :is_rational,
##                      :is_commutative)
## for prop in number_properties
##     prop_name = string(prop)
##     @eval ($prop)(ex::SymbolicObject) = PyCall.hasproperty(PyObject(ex), Symbol($prop_name)) &&  getproperty(PyObject(ex), Symbol($prop_name))
##     eval(Expr(:export, prop))
## end

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
`N` can take a precision argument.

When given as an integer greater than 16, we try to match the digits of accuracy using `BigFloat` precision on conversions to floating point.

"""
function N(x::Sym, digits::Int)
    ## check
    digits <= 16 && return(N(x))
    if is_integer(x) == nothing
        out = x.evalf(digits)
        return( N(out, digits) )
    end

    ex = x.evalf(digits)
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
        r, i = ex.as_real_imag()
        u, v = promote(N(r, digits), N(i, digits))
        return(Complex(u, v))
    end

    throw(DomainError())
end




## const relational_sympy_values = (:GreaterThan, :LessThan,
##                                  :StrictGreaterThan, :StrictLessThan,
##                                  :Equality, :Unequality)
## for meth in relational_sympy_values
##     meth_name = string(meth)
##     @eval begin
## #         @doc """
## # `$($meth_name)`: a SymPy function. [cf.](http://docs.sympy.org/dev/_modules/sympy/core/relational.html)
## # """ ->
##         ($meth)(a::Real, b::Real) = getproperty($sympy,$meth_name)(a, b)
##     end
## #    eval(Expr(:export, meth))
## end


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
