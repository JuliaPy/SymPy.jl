## matrix class stuff


## Matrix constructor
## There are issues, as for some reason we can't put Sym objects into an array
#Base.Array(::Type{Sym}, args...) = Base.Array(PyObject, args...)
#Sym{N}(o::Array{PyObject,N}) = Sym(sympy[:Matrix](o))
#Sym{T,N}(o::Array{T,N}) = Sym(convert(Array{PyObject,N}, o))
const SymMatrix = Sym

## map for linear indexing. Should be a function in base, but don't know it
function find_ijk(i, s)
    out = Integer[]
    while length(s) > 1
        p = prod(s[1:end-1])
        push!(out, iceil(i/p))
        i = (i-1) % prod(s[1:end-1]) +1
        s = s[1:end-1]
    end
        
    push!(out, i)
    tuple((reverse!(out) - 1)...)
end
## no linear indexing of matrices allowed here

getindex(s::SymMatrix, i::Integer...) = pyeval("x[i]", x=s.x, i= tuple(([i...]-1)...))
getindex(s::SymMatrix, i::Symbol) = project(s)[i] # is_nilpotent, ... many such predicates

## we want our matrices to be arrays of Sym objects so that julia manages them
## it is convenient (for printing, say) to convert to a sympy matrix
convert(SymMatrix, a::Array{Sym}) = Sym(sympy.Matrix(map(project, a)))
function convert(::Type{Array{Sym}}, a::SymMatrix)
    sz = size(a)
    ndims = length(sz)
    if ndims == 0
        a
    elseif ndims == 1
        Sym[a[i] for i in 1:length(a)]
    elseif ndims == 2
        Sym[a[i,j] for i in 1:size(a)[1], j in 1:size(a)[2]]
    else
        ## need something else for arrays... XXX -- can't linear index a
        b = Sym[a[find_ijk(i, sz)] for i in 1:length(a)]
        reshape(b, sz)
    end
end
    

## linear algebra functions that are methods of sympy.Matrix
## return a "scalar"
#for (nm, meth) in ((:det, "det"), )
for nm in (:det,
           :trace
           )
    cmd = "x." * string(nm) * "()"
    @eval ($nm)(a::SymMatrix) = Sym(pyeval(($cmd), x=project(a)))
    @eval ($nm)(a::Array{Sym, 2}) = ($nm)(convert(SymMatrix, a))
end

## return an array
for fn in (:inv,
           :adjoint, ## conj |> ctranspose
           :cholesky,
           :conjugate, ## conj
           :dual
           )
    ## alternate Sym(convert(SymMatrix,a)[:meth]()) |> u -> convert(Array{Sym}, u)
    cmd = "x." * string(fn) * "()"
    @eval ($fn)(a::SymMatrix) = convert(Array{Sym}, Sym(pyeval(($cmd), x=project(a))))
    @eval ($fn)(a::Array{Sym, 2}) = ($fn)(convert(SymMatrix, a))
end
conj(a::Sym) = conjugate(a)


## diagonalize??? returns (P,D)
## :eigenvals, returns {val => mult, val=> mult} ## eigvals
function eigvals(a::Array{Sym,2})
    d = convert(SymMatrix, a)[:eigenvals]()
    out = Sym[]
    for (k, v) in d
        for i in 1:v
            push!(out, Sym(k))
        end
    end
    out
end
## eigenvects ## returns list of triples (eigenval, multiplicity, basis).
function eigvecs(a::Array{Sym,2})
    d = convert(SymMatrix, a)[:eigenvects]()
    [{:eigenvalue=>Sym(u[1]), :multiplicity=>u[2], :basis=>map(x -> Sym(x), u[3])} for u in d]
end
    

function rref(a::Array{Sym, 2}; kwargs...)
    d = convert(SymMatrix, a)[:rref](; kwargs...)
    convert(Array{Sym}, convert(Sym, d[1]))
end

## call with a (A,b), return array
for fn in (:cross,
           :LUSolve, 
           :dot)
    cmd = "x." * string(fn) * "()"
    @eval ($fn)(A::SymMatrix, b::Sym) = convert(Array{Sym}, pyeval(($cmd), A=project(A), b=project(b)))
    @eval ($fn)(A::Array{Sym, 2}, b::Vector{Sym}) = $(fn)(convert(SymMatrix,A), convert(SymMatrix, b))
end

## GramSchmidt -- how to call?
## call with a (A,b), return scalar
# for fn in ()
#     @eval ($fn)(A::Sym, b::Sym) = convert(Array{Sym}, pyeval("A.($fn)(b)", A=project(A), b=project(b)))
#     @eval ($fn)(A::Array{Sym, 2}, b::Vector{Sym}) = ($fn)(convert(SymMatrix, A), convert(SymMatrix, b))
# end
    


## What other functions have these in common?
function jacobian(x::Array{Sym}, y::Array{Sym})
    X = convert(SymMatrix, x)
    Y = convert(SymMatrix, y)
    call_matrix_meth(X, :jacobian, Y)
end


## x, y = symbols("x y")
## f = x^2 - 2x*y
## hessian(f, [x,y])
function hessian(f::Sym, x::Array{Sym})
    out = call_meth(:hessian, f, x)
    convert(SymMatrix, out)
end