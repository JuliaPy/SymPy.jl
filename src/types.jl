## Symbol class for controlling dispatch

abstract SymbolicObject

## Basic types defined here
immutable Sym <: SymbolicObject
    x::PyCall.PyObject
end
Sym(s::SymbolicObject) = s


## Matrix type
immutable SymMatrix <: SymbolicObject
    x::PyCall.PyObject
end


## Automatic conversion of python types to Sym class.

basictype = sympy.basic["Basic"]
pytype_mapping(basictype, Sym)

polytype = sympy.polys["polytools"]["Poly"]
pytype_mapping(polytype, Sym)

convert(::Type{Sym}, o::PyCall.PyObject) = Sym(o)
convert(::Type{PyObject}, s::Sym) = s.x

function convert(::Type{Tuple}, o::PyCall.PyObject)
    ## check that o is a tuple?
    ## PyCall.pytypeof(o) 
    n = o[:__len__]()
    ntuple(n, i -> o[:__getitem__](i-1))
end


## convert SymPy matrices to SymMatrix
matrixtype = sympy.matrices["MatrixBase"]
pytype_mapping(matrixtype, SymMatrix)
convert(::Type{SymMatrix}, o::PyCall.PyObject) = SymMatrix(o)
convert(::Type{Sym}, o::SymMatrix) = Sym(o.x)
convert(::Type{SymMatrix}, o::Sym) = SymMatrix(o.x)
