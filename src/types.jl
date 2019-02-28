## Code for our Types and conversion methods


## Symbol class for controlling dispatch
abstract type SymbolicObject <: Number end

## Basic types defined here
"""

* `Sym("x")`
* `Sym(:x)`
* `Sym("x, y, z")`

The `Sym` type is an immutable type holding a reference to an
underlying python-based SymPy object. Many methods are extended to the
`Sym` type. Instances can be constructed in many ways. The one caveat
is the variables can not be function names in base.

"""
struct Sym <: SymbolicObject
    x::PyCall.PyObject
end
Sym(s::SymbolicObject) = s

## sets
struct SymSet <: SymbolicObject
    x::PyCall.PyObject
end


## complex float
## this cause issue with printing on non-complex objects
#mpctype = sympy.mpmath["ctx_mp_python"]
#pytype_mapping(mpctype, Sym)

## some typealiases
const SymOrReal =  Union{Sym,Real}
const SymOrNumber =  Union{Sym,Number}
const SymOrString =  Union{Sym,AbstractString}
const SymbolicTypes = Union{AbstractString,Symbol,SymbolicObject}


## in #83, @stevengj suggests using
PyCall.PyObject(x::SymbolicObject) = x.x

## Promotion
## promote up to symbolic so that math ops work
promote_rule(::Type{T}, ::Type{S})  where {T<:SymbolicObject, S<:Number}= T



## Conversion
convert(::Type{T}, o::PyCall.PyObject) where {T <: SymbolicObject} = T(o)
convert(::Type{PyObject}, s::Sym) = s.x


function convert(::Type{Tuple}, o::PyCall.PyObject)
    ## check that o is a tuple?
    ## PyCall.pytypeof(o)
    n = o.__len__()
    ntuple(i -> o.__getitem__(i-1), n)
end

## rational
convert(::Type{T}, x::Rational) where {T<:SymbolicObject} = sympy_meth(:Rational, x.num, x.den)::T

## big. Need mpmath installed separately -- not as a SymPy module as that is how it is called in PyCall
convert(::Type{T}, x::BigFloat) where {T<:SymbolicObject} = Sym(PyCall.PyObject(x))::T
convert(::Type{Sym}, x::Complex{BigFloat}) = Sym(PyCall.PyObject(x))::Sym

## real
convert(::Type{S}, x::T) where {S<:SymbolicObject, T <: Real}= sympy_meth(:sympify, x)::S
convert(::Type{T}, x::Sym) where {T <: Real} = convert(T, PyObject(x))


## complex
## cf math.jl for `complex` of a value
## IM is SymPy's "i" (sympy.I, not Python's
## Sym(PyCall.PyObject(im)) which gives 1j.
function convert(::Type{Sym}, x::Complex)
    y = ifelse(isa(x, Complex{Bool}), real(x) + imag(x) * im, x)
    real(y) + imag(y) * IM
end
convert(::Type{Complex{T}}, x::Sym) where {T} = complex(map(x -> convert(T, x), x.as_real_imag())...)
complex(::Type{Sym}) = Sym


## string
convert(::Type{Sym}, o::AbstractString) = sympy_meth(:sympify, o)
convert(::Type{Sym}, o::Symbol) = sympy_meth(:sympify, string(o))

## function
convert(::Type{Function}, ex::Sym) = lambdify(ex)

## we usually promote to Sym objects, but here we want to promote to functions
## so [x, sin] -> will be plottable as two functions
Base.promote_rule(::Type{T}, ::Type{S}) where {T<:SymbolicObject, S<:Function} = S
