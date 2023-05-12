## Matrix operations on SymMatrix

## * we support Array{Sym} using Julia's generic matrix functions
## * Array{Sym} allows calling of SymPy methods via the dot-call syntax (A.det())
## * we support SymPy's immutable matrices via the SymMatrix type. These use dot call style.




## for mapping sympy.Matrix -> Array{Sym} this is used
function Base.convert(::Type{Array{Sym,N}}, x::PyCall.PyObject) where {N}
    if PyCall.hasproperty(x, :__class__)
        nm = Symbol(x.__class__.__name__)
        _convert(Val(nm), x)
    else
        @show x
        ## error?
        x
    end
end

function _convert(::Val{:MutableDenseMatrix}, x)
    MM = x.tolist()
    length(size(MM)) == 2 && return MM
    sh = x.shape
    reshape(MM, sh)
end

function _convert(::Type{Val{T}}, x) where {T}
    @show :T
    x
end

function Base.convert(::Type{Array{T,N}}, M::SymMatrix) where {T <: SymbolicObject, N}
    MM = M.tolist()
    length(size(MM)) == N && return MM
    sh = M.shape
    reshape(MM,sh)
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

function Base.exp(A::Matrix{T}) where {T <: Sym}
    A.exp()
end

function LinearAlgebra.det(A::Matrix{T}) where {T <: Sym}
    A.det()
end

function LinearAlgebra.norm(A::AbstractArray{T,N}) where {T <: SymbolicObject,N}
    A.norm()
end

function _eigenvects(a::Matrix{Sym})
    ds =  a.eigenvects()
    ks = getindex.(ds, 1)
    d = Dict(u[1] => u[2:3] for u ∈ ds)
    ((k,d[k]...) for k ∈ sympy.ordered(ks))
end

function LinearAlgebra.eigvals(a::Matrix{Sym})
    out = Sym[]
    for eiv ∈ _eigenvects(a)
        for k ∈ 1:eiv[2]
            tmp = eiv[1]
            push!(out, tmp)
        end
    end
    out
end


function LinearAlgebra.eigvecs(a::Matrix{Sym})
    m = similar(a)
    j = 1
    for eiv ∈ _eigenvects(a)
        for k ∈ 1:eiv[2]
            m[:,j] .= eiv[3][k]
            j += 1
        end
    end
    m
end

# solve Ax=b for x, avoiding generic `lu`, which can be very slow for bigger n values
# fix suggested by @olof3 in issue 355
function LinearAlgebra.:\(A::AbstractArray{Sym,2}, b::AbstractArray{S,1}) where {S}

    m,n  = size(A)
    x =  Sym["x$i" for  i in 1:n]
    out = solve(A*x-b, x)
    isempty(out) && throw(SingularException(0)) # Could also return out here?
    ret = Vector{Sym}(undef, n)
    for (i,xᵢ)  in enumerate(x)
        ret[i] =  get(out,  xᵢ, xᵢ)
    end

    return ret

end

function LinearAlgebra.:\(A::AbstractArray{T,2}, B::AbstractArray{S,2}) where {T <: Sym, S}
    hcat([A \ bⱼ for bⱼ in eachcol(B)]...)
end

## Issue #359 so that A  +  λI is of type Sym
Base.:+(A::AbstractMatrix{T}, J::UniformScaling) where {T <: SymbolicObject} = (n=LinearAlgebra.checksquare(A); A .+ J.λ*I(n))
Base.:+(A::AbstractMatrix, J::UniformScaling{T}) where {T <: SymbolicObject} = (n=LinearAlgebra.checksquare(A); A .+ J.λ*I(n))
Base.:+(A::AbstractMatrix{T}, J::UniformScaling{T}) where {T <: SymbolicObject} = (n=LinearAlgebra.checksquare(A); A .+ J.λ*I(n))

Base.:-(J::UniformScaling, A::AbstractMatrix{T}) where {T <: SymbolicObject} = (-A) + J
Base.:-(J::UniformScaling{T}, A::AbstractMatrix) where {T <: SymbolicObject} = (-A) + J
Base.:-(J::UniformScaling{T}, A::AbstractMatrix{T}) where {T <: SymbolicObject} = (-A) + J


# Issue 397 so that A' infers correctly
Base.adjoint(x::Sym)::Sym = x.adjoint()

##################################################
##
## Support for ImmutableMatrix via SymMatrix

#### basic matrix operations must be delegated

## literal powers are tricky without this
Base.inv(x::SymMatrix) = x.inv()
*(x::SymMatrix, y::SymbolicObject) = x.multiply(y)
*(x::SymbolicObject, y::SymMatrix) = y.multiply(x)
*(x::SymMatrix, y::Number) = x.multiply(y)
*(x::Number, y::SymMatrix) = y.multiply(x)
*(x::SymMatrix, y::SymMatrix) = x.multiply(y)
/(x::SymMatrix, y::Number) = x.multiply(1/Sym(y))
/(x::SymMatrix, y::SymbolicObject) = x.multiply(1/y)
+(x::SymMatrix, y::SymMatrix) = x.add(y)
-(x::SymMatrix, y::SymMatrix) = x + y.multiply(-1)
^(x::SymMatrix, y::Union{Int, SymbolicObject}) = pycall(sympy.Pow, SymMatrix, x, y)

"""
    M[i,j]

Define `getindex` for SymPy's `ImmutableMatrix` class.

SymMatrix is 0-based, like python, not Julia. Use Matrix{Sym} for that.

"""
Base.getindex(M::SymMatrix, i::Int, j::Int) = M.__getitem__((i,j))
Base.getindex(V::SymMatrix, i::Int) = V.__getitem__((i,0))
Base.getindex(M::SymMatrix) = M

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
