## Symbol class for controlling dispatch

abstract SymbolicObject

## Basic types defined here
immutable Sym <: SymbolicObject
    x::PyCall.PyObject
end
Sym(s::SymbolicObject) = s

Base.start(x::Sym) = 1
Base.next(x::Sym, state) = (x.x, state-1)
Base.done(x::Sym, state) = state <= 0

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

## complex float
## this cause issue with printing on non-complex objects
#mpctype = sympy.mpmath["ctx_mp_python"]
#pytype_mapping(mpctype, Sym)


convert(::Type{Sym}, o::PyCall.PyObject) = Sym(o)
convert(::Type{PyObject}, s::Sym) = s.x

function convert(::Type{Tuple}, o::PyCall.PyObject)
    ## check that o is a tuple?
    ## PyCall.pytypeof(o) 
    n = o[:__len__]()
    ntuple(n, i -> o[:__getitem__](i-1))
end


## convert SymPy matrices to SymMatrix
try
    matrixtype = sympy.matrices["MatrixBase"]
    pytype_mapping(matrixtype, SymMatrix)
catch e
end
convert(::Type{SymMatrix}, o::PyCall.PyObject) = SymMatrix(o)
convert(::Type{Sym}, o::SymMatrix) = Sym(o.x)
convert(::Type{SymMatrix}, o::Sym) = SymMatrix(o.x)
