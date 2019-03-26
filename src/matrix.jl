## Matrix operations on SymMatrix

## * we support Array{Sym} using Julia's generic matrix functions
## * we support SymMatrix using SymPy's methods. These use dot call style.
## * we overload getproperty to allow dot call style on Array{Sym} objects

#### basic matrix operations must be delegated

## literal powers are tricky without this
Base.inv(x::SymMatrix) = x.inv()
*(x::SymMatrix, y::SymMatrix) = x.multiply(y)
*(x::Number, y::SymMatrix) = y.multiply(x)
*(x::SymMatrix, y::Number) = x.multiply(y)
/(x::SymMatrix, y::Number) = x.multiply(1/Sym(y))
/(x::SymMatrix, y::Sym) = x.multiply(1/y)
+(x::SymMatrix, y::SymMatrix) = x.add(y)
-(x::SymMatrix, y::SymMatrix) = x + y.multiply(-1)

#### Add 0-based getindex, setindex! methods
using OffsetArrays

"""
   M[i,j]

SymMatrix is 0-based, like python, not Julia. Use Matrix{Sym} for that.

Unlike the underlying SymPy object, `SymMatrix` objects are mutable and have 'getindex' and `setindex!` defined using 0-based indexing.

This is not performant.
"""
function Base.getindex(M::SymMatrix, i::Int, j::Int)
    N = M.tolist()[i+1, j+1]
    N
end
function Base.getindex(M::SymMatrix, I...)
    m, n = M.shape
    U = OffsetArray(convert(Matrix{Sym}, M), 0:m-1, 0:n-1)
    V = getindex(U, I...)
    # V is funny a bit, slices across rows return vectors too!
    if isa(V, AbstractArray)
        convert(SymMatrix, collect(V))
    else
        V
    end
end

# method for vectors, linear indexing
Base.getindex(V::SymMatrix, i::Int) = V[i,0]
Base.getindex(M::SymMatrix) = M

function Base.setindex!(M::SymMatrix, X, I...)
    sh = M.shape
    if length(sh) >= 2 && sh[2] > 1
        m,n = sh
        U = OffsetArray(convert(Matrix{Sym}, M), 0:m-1, 0:n-1)
        setindex!(U, X, I...)
        U1 = collect(U) #[U[i,j] for i in 0:(m-1), j in 0:(n-1)]
        M1 = convert(SymMatrix, U1)
    else
        n = sh[1]
        U = OffsetVector(convert(Vector{Sym}, M), 0:n-1)
        setindex!(U, X, I...)
        U1 = collect(U)
        M1 = convert(SymMatrix, U1)
    end
    copy!(M.x, M1.x)
end
Base.lastindex(M::SymMatrix, i::Int) = M.shape[i]


# Functions to convert between Matrix{Sym} and SymMatrix
function Base.convert(::Type{Matrix{T}}, M::SymMatrix) where {T <: SymbolicObject}
    M.tolist()
end
Base.convert(::Type{Vector{T}}, M::SymMatrix) where {T <: SymbolicObject} = M.tolist()[:,1]

function Base.convert(::Type{SymMatrix}, M::AbstractArray{T, N}) where {T <: Number, N}
    m,n = size(M)
    sympy.Matrix([PyCall.PyObject.(M[i,:]) for i in 1:m])
end

function Base.convert(::Type{SymMatrix}, V::Vector{T}) where {T <: Number}
    convert(SymMatrix, hcat(V))
end


## This allows abstract arrays of Sym Objects to slip through sympy.meth() calls
PyCall.PyObject(A::AbstractArray{Sym,2}) =
    PyCall.pycall(sympy.Matrix, PyCall.PyObject, [PyCall.PyObject.(A[i,:]) for i in 1:size(A)[1]])

PyCall.PyObject(V::AbstractArray{Sym,1}) =
    PyCall.pycall(sympy.Matrix, PyCall.PyObject,[[PyCall.PyObject(v)] for v in V])



# call SymMatrix method on Matrix{Sym}
## Eg. A.norm() where A = [x 1; 1 x], say
function Base.getproperty(A::AbstractArray{T}, k::Symbol) where {T <: SymbolicObject}
    if k in fieldnames(typeof(A))
        return getfield(A,k)
    else
        M = convert(SymMatrix, A)
        M1 = getproperty(M, k)
        M1
    end
end


## special case generic methods that fail on Array{Sym}:
function LinearAlgebra.norm(a::AbstractArray{Sym})
    a.norm()
end

function Base.inv(A::Array{T}) where {T <: SymbolicObject}
    convert(Matrix{T}, A.inv())
end

function LinearAlgebra.eigvals(a::Matrix{Sym})
    Sym[k for k in keys(a.eigenvals())]
end

function LinearAlgebra.eigvecs(a::Matrix{Sym})
    ds =  a.eigenvects()
    hcat((hcat((convert(Vector{Sym}, d) for d in di[3])...) for di in ds)...)
end
