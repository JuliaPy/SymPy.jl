## Matrix operations on SymMatrix

## we support Array{Sym} using Julia's generic matrix functions
## we support SymMatrix using SymPy's methods
## Here we define some helpers

#### basic matrix operations must be delegated

## literal powers are tricky without this
Base.inv(x::SymMatrix) = x.inv()
*(x::SymMatrix, y::SymMatrix) = x.multiply(y)
*(x::Number, y::SymMatrix) = y.multiply(x)
*(x::SymMatrix, y::Number) = x.multiply(y)
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

# method for vectors
Base.getindex(V::SymMatrix, i::Int) = V[i,0]

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

function Base.convert(::Type{SymMatrix}, M::Matrix{T}) where {T <: SymbolicObject}
    m,n = size(M)
    sympy.Matrix([M[i,:] for i in 1:m])
end

function Base.convert(::Type{SymMatrix}, V::Vector{T}) where {T <: SymbolicObject}
    convert(SymMatrix, hcat(V))
end

#PyCall.PyObject(M::Matrix{Sym}) = PyCall.PyObject(sympy.Matrix(M[i,:] for i in 1:size(M)[1]))
#
#    PyCall.PyObject(V::Vector{Sym}) = PyCall.PyObject(hcat(V)).x


## Generic methods fails
function LinearAlgebra.eigvals(a::Matrix{Sym})
    M = convert(SymMatrix, a)
    Sym[k for k in keys(M.eigenvals())]
end

function LinearAlgebra.eigvecs(a::Matrix{Sym})
    ds =  convert(SymMatrix,a).eigenvects()
    hcat((vcat((convert(Vector{Sym}, d) for d in di[3])...) for di in ds)...)
end
