## Matrix operations on SymMatrix

## * we support Array{Sym} using Julia's generic matrix functions
## * Array{Sym} allows calling of SymPy methods via the dot-call syntax (A.det())
## * we support SymPy's immutable matrices via the SymMatrix type. These use dot call style.




## for mapping sympy.Matrix -> Array{Sym} this is used
function Base.convert(::Type{Array{Sym}}, x::PyCall.PyObject)
    if PyCall.hasproperty(x, :__class__)
        nm = Symbol(x.__class__.__name__)
        _convert(Val(nm), x)
    else
        ## error?
        x
    end
end

function _convert(::Val{:MutableDenseMatrix}, x)
    sh = x.shape
    reshape(x.tolist(), sh)
end

function _convert(::Type{Val{T}}, x) where {T}
    x
end

function Base.convert(::Type{Array{T}}, M::SymMatrix) where {T <: SymbolicObject}
    sh = M.shape
    reshape(M.tolist(), sh)
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
        M1 = getproperty(PyCall.PyObject(A), k)
        M1
    end
end


##################################################
##
## special case generic methods that fail on Array{Sym}:

## inv, det may fail as lu need no pivoting specified in general
function Base.inv(A::Matrix{T}) where {T <: SymbolicObject}
    A.inv()
end

function LinearAlgebra.det(A::AbstractArray{T,N}) where {T <: SymbolicObject,N}
    A.det()
end

function LinearAlgebra.norm(A::AbstractArray{T,N}) where {T <: SymbolicObject,N}
    A.norm()
end

function LinearAlgebra.eigvals(a::Matrix{Sym})
    Sym[k for k in keys(a.eigenvals())]
end

function LinearAlgebra.eigvecs(a::Matrix{Sym})
    ds =  a.eigenvects()
    hcat((hcat(di[3]...) for di in ds)...)
end




##################################################
##
## Support for ImmutableMatrix via SymMatrix

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
^(x::SymMatrix, y::Union{Int, SymbolicObject}) = pycall(sympy.Pow, SymMatrix, x, y)

#### Add 0-based getindex, setindex! methods
using OffsetArrays

"""
   M[i,j]

Define `getindex` for SymPy's `ImmutableMatrix` class.

SymMatrix is 0-based, like python, not Julia. Use Matrix{Sym} for that.

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

## function Base.setindex!(M::SymMatrix, X, I...)
##     sh = M.shape
##     if length(sh) >= 2 && sh[2] > 1
##         m,n = sh
##         U = OffsetArray(convert(Matrix{Sym}, M), 0:m-1, 0:n-1)
##         setindex!(U, X, I...)
##         U1 = collect(U) #[U[i,j] for i in 0:(m-1), j in 0:(n-1)]
##         M1 = convert(SymMatrix, U1)
##     else
##         n = sh[1]
##         U = OffsetVector(convert(Vector{Sym}, M), 0:n-1)
##         setindex!(U, X, I...)
##         U1 = collect(U)
##         M1 = convert(SymMatrix, U1)
##     end
##     copy!(M.x, M1.x)
## end
Base.lastindex(M::SymMatrix, i::Int) = M.shape[i]

function Base.convert(::Type{SymMatrix}, M::AbstractArray{T, N}) where {T <: Number, N}
    m,n = size(M)
    sympy.ImmutableMatrix([PyCall.PyObject.(M[i,:]) for i in 1:m])
end

function Base.convert(::Type{SymMatrix}, V::Vector{T}) where {T <: Number}
    convert(SymMatrix, hcat(V))
end

## # Functions to convert between Matrix{Sym} and SymMatrix
function Base.convert(::Type{Array{Sym,N}}, M::SymMatrix) where {N}
    reshape(M.tolist(), M.shape)
end

function Base.convert(::Type{SymMatrix}, x::PyCall.PyObject)
    SymMatrix(x)
end
