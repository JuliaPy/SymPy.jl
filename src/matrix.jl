## matrix class stuff
## XXX work with Array{Sym}, not python array objects
## requires conversion from SymMatrix -> Array{Sym} in outputs, as appropriate

## covert back to Array{Sym}
subs(ex::SymMatrix, x::SymbolicObject, y) = convert(Array{Sym}, subs(convert(Sym, ex), x, y))


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
    tuple((reverse!(out) .- 1)...)
end


## no linear indexing of matrices allowed here
getindex(s::SymMatrix, i::Integer...) = pyeval("x[i]", x=s.x, i= tuple(([i...].-1)...))
getindex(s::SymMatrix, i::Symbol) = project(s)[i] # is_nilpotent, ... many such predicates
getindex(s::Array{Sym}, i::Symbol) = project(s)[i] # digaonalize..

## size
function size(x::SymMatrix)
    a = x[:shape]               # a PyObject tuple
    isa(a, PyObject) ? (a[:__getitem__](0), a[:__getitem__](1)) : a
end

function size(x::SymMatrix, dim::Integer)
    if dim <= 2 && pyisinstance(x.x, matrixtype)
        return x[:shape][dim]
    else
        return 1
    end
end

## we want our matrices to be arrays of Sym objects, not symbolic matrices
## so that julia manages them
## it is convenient (for printing, say) to convert to a sympy matrix
convert(::Type{SymMatrix}, a::Array{Sym}) = Sym(sympy.Matrix(map(project, a)))
convert(::Type{Sym}, a::Array{Sym}) = Sym(sympy.Matrix(map(project, a)))
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
  
## when projecting, we convert to a symbolic matrix thne project  
project(x::Array{Sym}) = convert(SymMatrix, x) |> project


## linear algebra functions that are methods of sympy.Matrix
## return a "scalar"
#for (nm, meth) in ((:det, "det"), )
for meth in (:condition_number,
           )

    cmd = "x." * string(meth) * "()"
    @eval ($meth)(a::SymMatrix) = Sym(pyeval(($cmd), x=project(a)))
    @eval ($meth)(a::Array{Sym, 2}) = ($meth)(convert(SymMatrix, a))
    eval(Expr(:export, meth))
end

for meth in (:det,
           :trace,
           :has,
           :norm
           )
    meth_name = string(meth)
    @eval ($meth)(a::SymMatrix, args...; kwargs...) = object_meth(a, symbol($meth_name), args...;kwargs...)
    @eval ($meth)(a::Array{Sym, 2}, args...; kwargs...) = ($meth)(convert(SymMatrix, a), args...;kwargs...)
    eval(Expr(:export, meth))
end



for meth in  (:is_anti_symmetric, :is_diagonal, :is_diagonalizable,:is_nilpotent, 
                :is_symbolic, :is_symmetric)
    
    meth_name = string(meth)
    @eval ($meth)(ex::SymMatrix, args...; kwargs...) = ex[symbol($meth_name)]()
    @eval ($meth)(ex::Array{Sym}, args...; kwargs...) = ex[symbol($meth_name)]()
    eval(Expr(:export, meth))
end


## methods called as properties
matrix_operators = (:H, :C,  
                    :is_lower, :is_lower_hessenberg, :is_square, :is_upper,  :is_upper_hessenberg, :is_zero
)

for meth in matrix_operators
     meth_name = string(meth)
     @eval ($meth)(ex::SymMatrix, args...; kwargs...) = ex[symbol($meth_name)]
     @eval ($meth)(ex::Array{Sym}, args...; kwargs...) = ex[symbol($meth_name)]
    eval(Expr(:export, meth))
end



## These take a matrix, return a container of symmatrices. Here we convert these to arrays of sym
map_matrix_methods = (:LDLsolve,
                      :LDLdecomposition, :LDLdecompositionFF,
                      :LUdecomposition_Simple,
                      :LUsolve,
                      :QRdecomposition, :QRsolve,
                      :adjoint, :adjugate,
                      :cholesky, :cholesky_solve, :cofactor, :conjugate, 
                      :diagaonal_solve, :diagonalize, :diff,:dual,
                      :expand,
                      :integrate, 
                      :inverse_ADJ, :inverse_GE, :inverse_LU,
                      :jordan_cells, :jordan_form,
                      :limit,
                      :lower_triangular_solve,
                      :minorEntry, :minorMatrix,
                      :n, :normalized, :nullspace,
                      :permuteBkwd, :permuteFwd,
                      :print_nonzero,
                      :singular_values,
                      :upper_triangular_solve,
                      :vec, :vech
                      )

for meth in map_matrix_methods
    meth_name = string(meth)
    @eval ($meth)(ex::SymMatrix, args...; kwargs...) = call_matrix_meth(ex, symbol($meth_name), args...; kwargs...)
    @eval ($meth)(ex::Array{Sym}, args...; kwargs...) = call_matrix_meth(convert(SymMatrix, ex), symbol($meth_name), args...; kwargs...)
    eval(Expr(:export, meth))
end


### Some special functions
exp(ex::Array{Sym}) = convert(Array{Sym}, object_meth(convert(SymMatrix, ex), :exp))
exp(a::SymMatrix) = a[:exp]()


Base.conj(a::SymMatrix) = conjugate(a)
Base.conj(a::Sym) = conjugate(a)


## :eigenvals, returns {val => mult, val=> mult} 
## we return an array of eigen values, as eigvals does
function eigvals(a::Array{Sym,2})
    ## this is a hack, as  d = a[:eigenvals]() may not convert to a Julia dict (Ubuntu...)
    ds = a[:eigenvects]()
    [d[1] for d in ds]
end
eigvals(a::SymMatrix) = eigvals(convert(Array{Sym}, a))

## :eigenvects ## returns list of triples (eigenval, multiplicity, basis).
function eigvecs(a::Array{Sym,2})
    ds = a[:eigenvects]()

    hcat([repmat(convert(Array{Sym}, d[3][1]), 1, d[2]) for d in ds]...)

#    [{:eigenvalue=>Sym(u[1]), :multiplicity=>u[2], :basis=>map(x -> Sym(x), u[3])} for u in d]
end
eigvecs(a::SymMatrix) = eigvecs(convert(Array{Sym}, a))   

function rref(a::Array{Sym, 2}; kwargs...)
    d = convert(SymMatrix, a)[:rref](; kwargs...)
    (convert(Array{Sym}, d[1]), d[2])
end
function rref(a::SymMatrix) 
  d = a[:rref]()
  (convert(Array{Sym}, d[1]), d[2]) ## return Array{Sym}, not SymMatrix
end

## call with a (A,b), return array
for fn in (:cross,
           :LUSolve)
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
    


# Jacobian
# example:
# rho, phi = @syms rho phi
# M = [rho*cos(phi), rho*sin(phi), rho^2]
# Y = [rho, phi]
# jacobian(M, Y)
function jacobian(x::Array{Sym}, y::Array{Sym})
    X = convert(SymMatrix, x)
    Y = convert(SymMatrix, y)
    call_matrix_meth(X, :jacobian, Y)
end
export jacobian

## x, y = symbols("x y")
## f = x^2 - 2x*y
## hessian(f, [x,y])
function hessian(f::Sym, x::Array{Sym})
    out = sympy_meth(:hessian, f, x)
    convert(SymMatrix, out) |> u -> convert(Array{Sym}, u)
end
export hessian

## dont' define inv -- it has amiguity with base inv
inverse(ex::SymMatrix) = call_matrix_meth(ex, :inv)
inverse(ex::Array{Sym}) = call_matrix_meth(convert(SymMatrix, ex),:inv)
export inverse


