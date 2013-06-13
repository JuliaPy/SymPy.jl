## Numbers. Should be a better way to make a value numeric
function convert{T <: Real}(::Type{Sym}, x::T)
    a = Sym(randstring(10))
    a | (a == x)
end


## Math Ops. May have this wrong.
## need to call through with pyeval here
+(x::Sym, y::Sym) =  Sym(pyeval("x + y", x = project(x), y = project(y)))
+(x::Sym, y::Real) = Sym(pyeval("x + y", x = project(x), y = project(y)))
+(x::Real, y::Sym) = Sym(pyeval("x + y", x = project(x), y = project(y)))
+(a::Array, x::Sym) = map(u -> u + x, a)
+(x::Sym, a::Array) = map(u -> x + u, a)

-(x::Sym, y::Sym) =  Sym(pyeval("x - y", x = project(x), y = project(y)))
-(x::Sym, y::Real) = Sym(pyeval("x - y", x = project(x), y = project(y)))
-(x::Real, y::Sym) = Sym(pyeval("x - y", x = project(x), y = project(y)))
-(a::Array, x::Sym) = map(u -> u - x, a)
-(x::Sym, a::Array) = map(u -> x - u, a)


-(x::Sym) = Sym(pyeval("-x", x = project(x)))

*(x::Sym, y::Sym) =  Sym(pyeval("x * y", x = project(x), y = project(y)))
*(x::Sym, y::Real) = Sym(pyeval("x * y", x = project(x), y = project(y)))
*(x::Real, y::Sym) = Sym(pyeval("x * y", x = project(x), y = project(y)))
*(x::Sym, a::Array) = map(u -> x*u, a)
*(a::Array, x::Sym) = map(u -> x*u, a)
.*{T <: Number}(x::T, y::Sym) = convert(Sym, x) * y
.*{T <: Number}(x::Sym, y::T) = x * convert(Sym, y)
.*(x::Sym, y::Sym) = convert(Sym, convert(Array{Sym}, x) .* convert(Array{Sym}, y))
.*(x::Sym, a::Array) = map(u -> x*u, a)
.*(a::Array, x::Sym) = map(u -> x*u, a)

/(x::Sym, y::Sym) =  Sym(pyeval("x / y", x = project(x), y = project(y)))
/(x::Sym, y::Real) = Sym(pyeval("x / y", x = project(x), y = project(y)))
/(x::Real, y::Sym) = Sym(pyeval("x / y", x = project(x), y = project(y)))
/(a::Array, x::Sym) = map(u -> u/x, a)
./(a::Array, x::Sym) = map(u -> u/x, a)
./(x::Sym, y::Sym) = convert(Sym, convert(Array{Sym}, x) ./ convert(Array{Sym}, y))
./{T <: Number}(x::T, y::Sym) = convert(Sym, x) / y
./{T <: Number}(x::Sym, y::T) = x / convert(Sym, y)

^(x::Sym, y::Sym) =     Sym(pyeval("x ** y", x = project(x), y = project(y)))
^(x::Sym, y::Integer) = Sym(pyeval("x ** y", x = project(x), y = project(y)))
^(x::Sym, y::Real) =    Sym(pyeval("x ** y", x = project(x), y = project(y)))
^(x::Real, y::Sym) =    Sym(pyeval("x ** y", x = project(x), y = project(y)))
.^(x::Sym, y::Sym) = convert(Array{Sym}, x) .^ convert(Array{Sym}, y)
.^{T <: Number}(x::T, y::Array{Sym}) = map(u -> x^u, y)
.^{T <: Number}(x::Array{Sym}, y::T) = map(u -> u^y, x)



## Math functions       
for fn in (:sin, :cos, :tan, :sinh, :cosh, :tanh, :asin, :acos, :atan,
           :asinh, :acosh, :atanh, :sec, :csc, :cot, :asec, :acsc, :acot, 
           :sech, :csch, :coth, :asech, :acsch, :acoth, :sinc, :cosc, 
           :cosd, :cotd, :cscd, :secd, :sind, :tand,
           :acosd, :acotd, :acscd, :asecd, :asind, :atand, :atan2,
           :radians2degrees, :degrees2radians,
           :log, :log2, :log10, :log1p, :exponent, :exp, :exp2, :expm1,
           :cbrt, :sqrt, :square, :erf, :erfc, :erfcx, :erfi, :dawson,
           :ceil, :floor, :trunc, :round, :significand
           )


    meth = string(fn)
    @eval ($fn)(x::Sym) = convert(Sym, sympy[symbol($meth)](project(x)) )
    @eval ($fn)(a::Array{Sym}) = map($fn, a)
end


    


## add
abs(x::Sym) = Sym(sympy[:Abs](project(x)))
abs(a::Array{Sym}) = map(abs, a)

## Make a function argument, but munge arguments from Sym -> PyObject class
SymFunction(nm::Union(Symbol, String)) = (args...) -> Sym(sympy[:Function](nm)(project(args)...))

## Some sympy function interfaces

## subs
function subs(ex::Sym, x::Sym, arg)
    convert(Sym, project(ex)[:subs](project(x), project(arg)))
end
subs(exs::Array{Sym}, x::Sym, arg) = map(ex->subs(ex, x, arg), exs)

## This is *experimental* syntax to lessen the typing of subs
## THis would work  ex | (x == 2) --> subs(ex, x, 2)
=={T <: Union(Real, Complex)}(x::Sym, y::T) = (ex) -> subs(ex, x, y)
function !={T <: Real}(x::Sym, y::T) 
    try 
        x = float(x)
        x != y
    catch
        false
    end
end
function !={T <: Complex}(x::Sym, y::T) 
    try 
        x = complex(x)
        x != y
    catch
        false
    end
end


for meth in (:n,
             :simplify, :nsimplify, :factor, :collect, :separate,
             :radsimp, :ratsimp,  :trigsimp, :powsimp, :combsimp, :hypersimp,
             :primitive, :gcd, :resultant, :cancel,
             :expand, :together, :apart,
             :limit, :diff,
             :series, :integrate)
    meth_name = string(meth)
    @eval ($meth)(args...; kwargs...) = call_meth(symbol($meth_name), args...; kwargs...)
end

## different conversions
fraction(args...; kwargs...) = sympy.fraction(project(args)...; kwargs...) | os -> map(u -> convert(Sym, u), os)




## Alternate interface for simple integral
integrate(s::Sym, x::Sym, from::Real, to::Real) = integrate(s, (x, from, to))

## special numbers
I = Sym(sympy[:I])
oo = Sym(sympy[:oo])

## Special functions
## Spherical harmonic

## functions which are methods of sympy, not a symbolic instance
for fn in (:summation,
           :Ylm, :factorial, :gamma, :beta,
           :assoc_legendre, :chebyshevt, :legendre, :hermite
           )
    meth = string(fn)
    
    @eval ($fn)(args...) = Sym(sympy[symbol($meth)](project(args)...))
end

## Polynomial functions. There are many, but here we have
poly(s::Sym) = Sym( sympy[:poly](project(s)) )
real_roots(s::Sym) = [convert(Real, u) for u in sympy[:real_roots](project(s))] | float
nroots(s::Sym) = [Sym(u) for u in sympy[:roots](project(s))]

## solve

## Is this a good idea? I want to be able to solve equations with
## solve(x^2 +x == x, x)
==(x::Sym, y::Sym) = solve(x - y)


## solve. Returns array of PyObjects
## Trying to return an array of Sym objects printed funny!
function solve(ex::Sym, x::Sym, args...)
    out = Array(Sym, 0)
    [push!(out, Sym(u)) for u in sympy.solve(project(ex), project(x), project(args)...)]
    out
end
function solve(ex::Sym)
    out = Array(Sym, 0)
    [push!(out, Sym(u)) for u in sympy.solve(project(ex))]
    out
end
function solve(exs::Vector{Sym}, xs::Vector{Sym})
    sympy[:solve](map(project, exs), map(project, xs)) #  dictionary with keys, values as PyObjects
end

## dsolve
##  A little trickier to use
## f = SymFunction("f")
## x = Sym("x")
## dsolve(diff(f(x), x) + f(x), f(x)) ## solve f'(x) + f(x) = 0
## dsolve(diff(f(x), x, x) + f(x), f(x)) ## solve f''(x) + f(x) = 0
dsolve(ex::Sym, fx::Sym) = Sym(sympy[:dsolve]( project(ex), project(fx)))

## Matrix constructor
## There are issues, as for some reason we can't put Sym objects into an array
#Base.Array(::Type{Sym}, args...) = Base.Array(PyObject, args...)
#Sym{N}(o::Array{PyObject,N}) = Sym(sympy[:Matrix](o))
#Sym{T,N}(o::Array{T,N}) = Sym(convert(Array{PyObject,N}, o))
const SymMatrix = Sym


getindex(s::SymMatrix, i::Integer...) = Sym(pyeval("x[i]", x=s.x, i=tuple(([i...]-1)...)))
getindex(s::SymMatrix, i::Symbol) = project(s)[i] # is_nilpotent, ... many such predicates

## we want our matrices to be arrays of Sym objects so that julia manages them
## it is convenient (for printing, say) to convert to a sympy matrix
convert(SymMatrix, a::Array{Sym}) = Sym(sympy.Matrix(map(project, a)))
function convert(::Type{Array{Sym}}, a::SymMatrix)
    ndims = length(size(a))
    if ndims == 0
        a
    elseif ndims == 1
        Sym[a[i] for i in 1:length(a)]
    elseif ndims == 2
        Sym[a[i,j] for i in 1:size(a)[1], j in 1:size(a)[2]]
    else
        b = Sym[a[i] for i in 1:length(a)]
        reshape(b, size(a))
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
           :adjoint, ## conj | ctranspose
           :cholesky,
           :conjugate, ## conj
           :dual, 
           :exp)
    ## alternate Sym(convert(SymMatrix,a)[:meth]()) | u -> convert(Array{Sym}, u)
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
    


