## matrix class stuff
## Work with Array{Sym}, not python array objects, as possible

## The basic idea is: we use pytype mapping to make sympy matrices into Array{Sym}.
## this is achieved with the convert(Array{Sym}, o::PyObject) method below.

## Array{Sym} objects are converted into Python objects via
PyCall.PyObject(a::Array{Sym}) = pycall(sympy[:Matrix], PyObject, PyCall.array2py(a))

## Matrix methods and objects


## For calling methods we have  call_matrix_meth(M, :meth, ...) for M.meth(...)
## This helps, grabbing the M.meth part
function getindex(s::Array{Sym}, i::Symbol)
    PyObject(s)[i]
end


# call a matrix method M[:det](). Matrix or vector arguments are converted via symmatrix
# though this may need to be done specially for some arguments that are passed in as collections
global call_matrix_meth(object, meth, args...; kwargs...) = begin
    o = PyObject(object)
    o[meth](args...; kwargs...)
end


## support for convert(Array{Sym}, o::PyObject)
function matrix_size(x::PyObject)
    a = x[:shape]            # tuple
    if isa(a, PyObject)
        a =  (a[:__getitem__](0), a[:__getitem__](1))
    end
    while a[end] == 1
        a = a[1:end-1]
    end
    a
end

get_matrix_index(s::PyObject, i::Integer...) = get(s, Sym, ntuple(k -> i[k]-1, length(i)))
function get_matrix_index(s::PyObject, i::Integer)
    ind = ind2sub(matrix_size(s), i-1) # 0-base PyThon, 1 base Julia
    if length(ind) == 1
        get(s, Sym, ind[1])
    else
        get(s, Sym, ind)
    end
end

function convert(::Type{Array{Sym}}, o::PyObject)
    sz = matrix_size(o)
    ndims = length(sz)
    if ndims == 0
        Sym(o)
    elseif ndims == 1
        Sym[get_matrix_index(o,i) for i in 1:sz[1]]
    elseif ndims == 2
        Sym[get_matrix_index(o,i,j) for i in 1:sz[1], j in 1:sz[2]]
    else
        ## need something else for arrays... XXX -- can't linear index a
        b = Sym[get_matrix_index(o,i) for i in 1:prod(sz)]
        reshape(b, sz)
    end
end



## covert back to Array{Sym}. Could just use broadcast (subs.(...)) here
## once v0.4 support is dropped.
subs(ex::Array{Sym}, args...; kwargs...) = map(u -> subs(u, args...; kwargs...), ex)


## Methods

## these are sympy methods that need exporting
sympy_matrix_methods = (:jordan_cell,
                        )

for meth in sympy_matrix_methods
    meth_name=string(meth)
    @eval begin
        @doc """
`$($meth_name)`: a SymPy function.
The SymPy documentation can be found through: http://docs.sympy.org/latest/search.html?q=$($meth_name)

Specific docs may also be found at [SymPy Docs for matrices](http://docs.sympy.org/latest/modules/matrices/matrices.html#module-sympy.matrices.matrices)
""" ->
         ($meth)(args...; kwargs...) = sympy_meth(@compat(Symbol($meth_name)), args...; kwargs...)
    end
    eval(Expr(:export, meth))
end


## These are matrix methods that need exporting
matrix_methods = (:LDLsolve,
                  :LDLdecomposition, :LDLdecompositionFF,
                  :LUdecomposition_Simple,
                  :LUdecomposition,
                  :LUsolve,
                  :QRdecomposition, :QRsolve,
                  :adjoint, :adjugate,
                  :cholesky, :cholesky_solve, :conjugate,
                  :diagonal_solve, :diagonalize, :dual,
                  :expand,
                  :integrate,
                  :is_symmetric,
                  :inverse_ADJ, :inverse_GE, :inverse_LU,
                  :jordan_form,
                  :limit,
                  :lower_triangular_solve,
                  :minorEntry, :minorMatrix,
                  :normalized, :nullspace,
                  :permuteBkwd, :permuteFwd,
                  :print_nonzero,
                  :rref,
                  :singular_values,
                  :upper_triangular_solve,
                  :vech
)

for meth in matrix_methods
    meth_name = string(meth)
    @eval begin
      @doc """
`$($meth_name)`: a SymPy function.
The SymPy documentation can be found through: http://docs.sympy.org/latest/search.html?q=$($meth_name)

Specific docs may also be found at [SymPy Docs for matrices](http://docs.sympy.org/latest/modules/matrices/matrices.html#module-sympy.matrices.matrices)
""" ->
        ($meth)(ex::Matrix{Sym}, args...; kwargs...) =  call_matrix_meth(ex, @compat(Symbol($meth_name)), args...;kwargs...)
    end
    eval(Expr(:export, meth))
end


cofactor(A::Matrix{Sym}, i, j) = call_matrix_meth(A, :cofactor, i-1, j-1)
jacobian(X::Array{Sym}, Y::Array{Sym}) = call_matrix_meth(X, :jacobian, Y)

export cofactor

## These are SymPy properties that we want to call as methods, exported
matrix_properties = (:H, :C,
                    :is_lower, :is_lower_hessenberg, :is_square, :is_upper,
                    :is_upper_hessenberg, :is_zero
                     )


for meth in matrix_properties
     meth_name = string(meth)
    @eval begin
      @doc """
`$($meth_name)`: a SymPy function.
The SymPy documentation can be found through: http://docs.sympy.org/latest/search.html?q=$($meth_name)
""" ->
        ($meth)(ex::Matrix{Sym}, args...; kwargs...) =  ex[@compat(Symbol($meth_name))]
    end
    eval(Expr(:export, meth))
end


## These are special cased
norm(a::Array{Sym}, args...; kwargs...) = call_matrix_meth(a, :norm, args...; kwargs...)
chol(a::Matrix{Sym}) = cholesky(a)
expm(ex::Matrix{Sym}) = convert(Array{Sym}, expm(convert(SymMatrix, ex)))
conj(a::Sym) = conjugate(a)
eigvals(a::Matrix{Sym}) = collect(keys(call_matrix_meth(a, :eigenvals))) # a[:eigevnals]() has multiplicity
function eigvecs(a::Matrix{Sym})
    ds =  call_matrix_meth(a, :eigenvects)
    out = Any[]
    for d in ds
        append!(out, d[3])
    end
    out
end
rref{T <: Integer}(a::Matrix{T}) = N(rref(convert(Matrix{Sym}, a)))
rref{T <: Integer}(a::Matrix{Rational{T}}) = N(rref(convert(Matrix{Sym}, a)))

"""
Return orthogonal basis from a set of vectors

Example:
```
L = [Sym[1,2,3], Sym[2,5,9], Sym[1,4,2]] # need Sym vectors.
GramSchmidt(L, true)
```
"""
GramSchmidt{T}(vecs::Vector{Vector{T}}, args...; kwargs...) = sympy_meth(:GramSchmidt, map(u->convert(Vector{Sym},u),vecs), args...; kwargs...)
## :cross?
## hessian, wronskian, GramSchmidt

"""

Find Hessian of a symbolic expression

Example:
```
x, y = symbols("x y")
f = x^2 - 2x*y
hessian(f, [x,y])

u = SymFunction("u")(x,y)
hessian(u, [x,y])
```
"""
hessian(u::SymbolicObject, vars::Array{Sym}, args...) = sympy_meth(:hessian, u, vars, args...)
hessian(u::SymbolicObject) = hessian(u, free_symbols(u))


"""
Wronskian of functions

Example
```
u = SymFunction("u")(x)
v = SymFunction("v")(x)
wronskian([u,v], x)  # determinant of [u v; u' v']
```

"""
wronskian{T <: SymbolicObject}(fs::Vector{T}, x::Sym, args...; kwargs...) = sympy_meth(:wronskian, fs, x, args...; kwargs...)

export GramSchmidt, hessian, wronskian
