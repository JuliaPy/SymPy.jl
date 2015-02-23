## Symbol class for controlling dispatch

abstract SymbolicObject <: Number

## Basic types defined here
immutable Sym <: SymbolicObject
    x::PyCall.PyObject
end
Sym(s::SymbolicObject) = s

## Matrix type
immutable SymMatrix <: SymbolicObject
    x::PyCall.PyObject
end

convert(::Type{Sym}, o::String) = Sym(o)
convert(::Type{Sym}, o::Number) = Sym(o)

## Automatic conversion of python types to Sym class.

basictype = sympy.basic["Basic"]
pytype_mapping(basictype, Sym)

polytype = sympy.polys["polytools"]["Poly"]
pytype_mapping(polytype, Sym)

try
    matrixtype = sympy.matrices["MatrixBase"]
    pytype_mapping(matrixtype, SymMatrix)
catch e
end

## complex float
## this cause issue with printing on non-complex objects
#mpctype = sympy.mpmath["ctx_mp_python"]
#pytype_mapping(mpctype, Sym)

## Iterator for Sym
Base.start(x::Sym) = 1
Base.next(x::Sym, state) = (x.x, state-1)
Base.done(x::Sym, state) = state <= 0


## Conversion methods
convert(::Type{Sym}, o::String) = sympy.sympify(o)
convert(::Type{Sym}, o::PyCall.PyObject) = Sym(o)
convert(::Type{PyObject}, s::Sym) = s.x
function convert(::Type{Tuple}, o::PyCall.PyObject)
    ## check that o is a tuple?
    ## PyCall.pytypeof(o) 
    n = o[:__len__]()
    ntuple(n, i -> o[:__getitem__](i-1))
end
convert{T<:SymbolicObject}(::Type{T}, x::Rational) = sympy.Rational(x.num, x.den)
convert{S<:SymbolicObject, T <: Real}(::Type{S}, x::T) = sympy.sympify(x)
convert(::Type{Sym}, x::Complex) = real(x) == 0 ? sympy.Symbol("$(imag(x))*I") : sympy.Symbol("$(real(x)) + $(imag(x))*I")

convert(::Type{Complex}, x::Sym) = complex(map(float, x[:as_real_imag]())...)
complex(x::Sym) = convert(Complex, x)
complex(xs::Array{Sym}) = map(complex, xs)

convert(::Type{SymMatrix}, o::PyCall.PyObject) = SymMatrix(o)
convert(::Type{Sym}, o::SymMatrix) = Sym(o.x)
convert(::Type{SymMatrix}, o::Sym) = SymMatrix(o.x)



# convert(::Type{Sym}, o::PyCall.PyObject) = Sym(o)
# convert(::Type{PyObject}, s::Sym) = s.x

# function convert(::Type{Tuple}, o::PyCall.PyObject)
#     ## check that o is a tuple?
#     ## PyCall.pytypeof(o) 
#     n = o[:__len__]()
#     ntuple(n, i -> o[:__getitem__](i-1))
# end


## convert SymPy matrices to SymMatrix
convert(::Type{SymMatrix}, o::PyCall.PyObject) = SymMatrix(o)
convert(::Type{Sym}, o::SymMatrix) = Sym(o.x)
convert(::Type{SymMatrix}, o::Sym) = SymMatrix(o.x)
