##################################################
## SymbolicObject types have field x::PyCall.PyObject

## Symbol class for controlling dispatch
abstract type SymbolicObject <: Number end
struct Sym <: SymbolicObject
    __pyobject__::PyCall.PyObject
end




## Matrices
## We use this class for `ImmutableMatrices`
## Mutable matrices are mapped to `AbstractArray{Sym,N}`
## cf. matrix.jl
"""
    SymMatrix

Type to store a SymPy matrix, as created by `sympy.ImmutableMatrix`.

These have 0-based indexing defined for them to match SymPy

The traditional infix mathmatical operations are defined, but no dot broadcasting.

The `convert(Matrix{Sym}, M)` call is useful to covert to a Julia matrix

"""
mutable struct SymMatrix <: SymbolicObject
    __pyobject__::PyCall.PyObject
end

## Permutations
## A permutation of {0, 1, 2, ..., n} -- 0-based
struct SymPermutation <: SymbolicObject
  __pyobject__::PyCall.PyObject
end
export SymPermutation
Base.convert(::Type{SymPermutation}, o::PyCall.PyObject) = SymPermutation(o)


## A permutation of {0, 1, 2, ..., n} -- 0-based
struct SymPermutationGroup <: SymbolicObject
  __pyobject__::PyCall.PyObject
end
export SymPermutationGroup
Base.convert(::Type{SymPermutationGroup}, o::PyCall.PyObject) = SymPermutationGroup(o)

# a Lambda function
struct Lambda <: SymbolicObject
    __pyobject__::PyCall.PyObject
end
Lambda(args, expression) = Lambda(sympy.Lambda(args, expression).__pyobject__)
(λ::Lambda)(args...; kwargs...) = λ.__pyobject__(args...; kwargs...)
export Lambda


##################################################

## important override
## this allows most things to flow though PyCall
PyCall.PyObject(x::SymbolicObject) = x.__pyobject__
## Override this so that using symbols as keys in a dict works
hash(x::SymbolicObject) = hash(PyObject(x))
==(x::SymbolicObject, y::SymbolicObject) = PyObject(x) == PyObject(y)

##################################################


## Show methods
"create basic printed output"
function jprint(x::SymbolicObject)
    out = PyCall.pycall(pybuiltin("str"), String, PyObject(x))
    out = replace(out, r"\*\*" => "^")
    out
end
jprint(x::AbstractArray) = map(jprint, x)

## text/plain
Base.show(io::IO, s::Sym) = print(io, jprint(s))
Base.show(io::IO, ::MIME"text/plain", s::SymbolicObject) =  print(io, sympy.pretty(s))

## latex enhancements: Sym, array, Dict
Base.show(io::IO, ::MIME"text/latex", x::SymbolicObject) = print(io, sympy.latex(x, mode="equation*"))

function  show(io::IO, ::MIME"text/latex", x::AbstractArray{Sym})
    function toeqnarray(x::Vector{Sym})
        a = join([sympy.latex(x[i]) for i in 1:length(x)], "\\\\")
        """\\[ \\left[ \\begin{array}{r}$a\\end{array} \\right] \\]"""
#        "\\begin{bmatrix}$a\\end{bmatrix}"
    end
    function toeqnarray(x::AbstractArray{Sym,2})
        sz = size(x)
        a = join([join(map(sympy.latex, x[i,:]), "&") for i in 1:sz[1]], "\\\\")
        "\\[\\left[ \\begin{array}{" * repeat("r",sz[2]) * "}" * a * "\\end{array}\\right]\\]"
#        "\\begin{bmatrix}$a\\end{bmatrix}"
    end
    print(io, toeqnarray(x))
end
function show(io::IO, ::MIME"text/latex", d::Dict{T,S}) where {T<:SymbolicObject, S<:Any}
    Latex(x::Sym) = sympy.latex(x)
    Latex(x) = sprint(io -> show(IOContext(io, :compact => true), x))

    out = "\\begin{equation*}\\begin{cases}"
    for (k,v) in d
        out = out * Latex(k) * " & \\text{=>} &" * Latex(v) * "\\\\"
    end
    out = out * "\\end{cases}\\end{equation*}"
    print(io, out)
end

latex(x::Sym) = sympy.latex(x)



## Following recent changes to PyCall where:
# For o::PyObject, make o["foo"], o[:foo], and o.foo equivalent to o.foo in Python,
# with the former returning an raw PyObject and the latter giving the PyAny
# conversion.
# We do something similar to SymPy
#
# We only implement for symbols here, not strings
function Base.getproperty(o::T, s::Symbol) where {T <: SymbolicObject}
    if (s in fieldnames(T))
        getfield(o, s)
    else
        getproperty(PyCall.PyObject(o), s)
    end
end
