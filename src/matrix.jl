## matrix class stuff
## XXX work with Array{Sym}, not python array objects
## requires conversion from SymMatrix -> Array{Sym} in outputs, as appropriate

## covert back to Array{Sym}
function subs(ex::Array{Sym}, args...; kwargs...)
    u = convert(SymMatrix, ex)
    convert(Array{Sym}, subs(u, args...; kwargs...))
end



getindex(s::SymMatrix, i::Integer...) = pyeval("x[i]", x=s.x, i= tuple(([i...].-1)...))
getindex(s::SymMatrix, i::Integer) = pyeval("x[i]", x=project(s), i=map(x->x-1, ind2sub(size(s), i)))
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
convert(::Type{SymMatrix}, a::Array{Sym}) = Sym(sympy[:Matrix](map(project, a)))
convert(::Type{Sym}, a::Array{Sym}) = Sym(sympy[:Matrix](map(project, a)))
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
        b = Sym[a[i] for i in 1:length(a)]
        reshape(b, sz)
    end
end
  
## when projecting, we convert to a symbolic matrix then project  
project(x::Array{Sym}) = convert(SymMatrix, x) |> project


## linear algebra functions that are methods of sympy.Matrix
## return a "scalar"
for meth in (:condition_number,
           )

    cmd = "x." * string(meth) * "()"
    @eval ($meth)(a::SymMatrix) = Sym(pyeval(($cmd), x=project(a)))
    @eval ($meth)(a::Matrix{Sym}) = ($meth)(convert(SymMatrix, a))
    eval(Expr(:export, meth))
end

## can be any dimension
for meth in (:norm,
             )
    meth_name = string(meth)
    @eval ($meth)(a::SymMatrix, args...; kwargs...) = object_meth(a, symbol($meth_name), args...;kwargs...)
    @eval ($meth)(a::Array{Sym}, args...; kwargs...) = ($meth)(convert(SymMatrix, a), args...;kwargs...)
    eval(Expr(:export, meth))
end
    
for meth in (
           :has,
           )
    meth_name = string(meth)
    @eval ($meth)(a::SymMatrix, args...; kwargs...) = object_meth(a, symbol($meth_name), args...;kwargs...)
    @eval ($meth)(a::Matrix{Sym}, args...; kwargs...) = ($meth)(convert(SymMatrix, a), args...;kwargs...)
    eval(Expr(:export, meth))
end

## dont' define inv -- it has amiguity with base inv
inverse(ex::SymMatrix) = call_matrix_meth(ex, :inv)
inverse(ex::Matrix{Sym}) = call_matrix_meth(convert(SymMatrix, ex),:inv)
export inverse


if VERSION < v"0.4.0-dev"
    det(x::Matrix{Sym}) = det(lufact(x, pivot=false))
    inv(x::Matrix{Sym}) = inverse(x)
end

## But
## is_symmetric <-> issym
## istriu, istril, 
for meth in  (:is_anti_symmetric, :is_diagonal, :is_diagonalizable,:is_nilpotent, 
                :is_symbolic, :is_symmetric)
    
    meth_name = string(meth)
    @eval ($meth)(ex::SymMatrix, args...; kwargs...) = ex[symbol($meth_name)]()
    @eval ($meth)(ex::Matrix{Sym}, args...; kwargs...) = ex[symbol($meth_name)]()
    eval(Expr(:export, meth))
end


## methods called as properties

## is_upper <-> itriu
## is_lower <-> istril
matrix_operators = (:H, :C,  
                    :is_lower, :is_lower_hessenberg, :is_square, :is_upper,  :is_upper_hessenberg, :is_zero
)

for meth in matrix_operators
     meth_name = string(meth)
     @eval ($meth)(ex::SymMatrix, args...; kwargs...) = ex[symbol($meth_name)]
     @eval ($meth)(ex::Matrix{Sym}, args...; kwargs...) = ex[symbol($meth_name)]
    eval(Expr(:export, meth))
end



## These take a matrix, return a container of symmatrices. Here we convert these to arrays of sym
## Could use
## - qr generically instead of QRdecomposition
## - lu instead of
## - chol will call cholesky
map_matrix_methods = (:LDLsolve,
                      :LDLdecomposition, :LDLdecompositionFF,
                      :LUdecomposition_Simple,
                      :LUsolve,
                      :QRdecomposition, :QRsolve,
                      :adjoint, :adjugate,
                      :cholesky, :cholesky_solve, :cofactor, :conjugate, 
                      :diagaonal_solve, :diagonalize, :dual,
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
                      :vech
                      )

for meth in map_matrix_methods
    meth_name = string(meth)
    @eval ($meth)(ex::SymMatrix, args...; kwargs...) = call_matrix_meth(ex, symbol($meth_name), args...; kwargs...)
    @eval ($meth)(ex::Matrix{Sym}, args...; kwargs...) = call_matrix_meth(convert(SymMatrix, ex), symbol($meth_name), args...; kwargs...)
    eval(Expr(:export, meth))
end


### Some special functions
Base.chol(a::Matrix{Sym}) = cholesky(a)

expm(ex::Matrix{Sym}) = convert(Array{Sym}, object_meth(convert(SymMatrix, ex), :exp))
expm(a::SymMatrix) = a[:exp]()

Base.conj(a::SymMatrix) = conjugate(a)
Base.conj(a::Sym) = conjugate(a)


## :eigenvals, returns {val => mult, val=> mult} 
## we return an array of eigen values, as eigvals does
function eigvals(a::Matrix{Sym})
    ## this is a hack, as  d = a[:eigenvals]() may not convert to a Julia dict (Ubuntu...)
    ds = a[:eigenvects]()
    [d[1] for d in ds]
end
eigvals(a::SymMatrix) = eigvals(convert(Array{Sym}, a))

## :eigenvects ## returns list of triples (eigenval, multiplicity, basis).
"""

The `eigvecs` function returns a list of triples (eigenval, multiplicity, basis) for an `Matrix{Sym}`.

"""
function eigvecs(a::Matrix{Sym})
    ds = a[:eigenvects]()

    hcat([hcat([convert(Array{Sym}, v) for v in d[3]]...) for d in ds]...)

end
eigvecs(a::SymMatrix) = eigvecs(convert(Array{Sym}, a))   

## Take any matrix and return reduced row-echelon form and indices of pivot vars
## To simplify elements before finding nonzero pivots set simplified=True
## We return only the rref form -- not the pivot vars.
"""

Return reduced row echelon form. This functino does not return the pivot variables generated by SymPy.

"""
function rref(a::Matrix{Sym}; kwargs...)
    d = convert(SymMatrix, a)[:rref](; kwargs...)
    convert(Array{Sym}, d[1])
end

## rref. The sympy method returns 
function rref(a::SymMatrix) 
  d = a[:rref]()
  convert(Array{Sym}, d[1]) ## return Array{Sym}, not SymMatrix
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
function jacobian(x::Matrix{Sym}, y::Matrix{Sym})
    X = convert(SymMatrix, x)
    Y = convert(SymMatrix, y)
    call_matrix_meth(X, :jacobian, Y)
end
export jacobian

## x, y = symbols("x y")
## f = x^2 - 2x*y
## hessian(f, [x,y])
"Find Hessian of a symbolic expression"
function hessian(f::Sym, x::Vector{Sym})
    out = sympy_meth(:hessian, f, x)
    convert(SymMatrix, out) |> u -> convert(Array{Sym}, u)
end
hessian(ex::Sym) = hessian(ex, get_free_symbols(ex))
export hessian

## For gradient we have [diff(ex,var) for var in get_free_symbols(ex)]... but order is of importance...


    



