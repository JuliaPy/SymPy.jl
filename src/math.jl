## Math functions

+(x::Sym, y::Sym) =  Sym(pyeval("x + y", x = project(x), y = project(y)))
+(x::Sym, y::Real) = Sym(pyeval("x + y", x = project(x), y = project(y)))
+(x::Real, y::Sym) = Sym(pyeval("x + y", x = project(x), y = project(y)))

-(x::Sym, y::Sym) =  Sym(pyeval("x - y", x = project(x), y = project(y)))
-(x::Sym, y::Real) = Sym(pyeval("x - y", x = project(x), y = project(y)))
-(x::Real, y::Sym) = Sym(pyeval("x - y", x = project(x), y = project(y)))


-(x::Sym) = Sym(pyeval("-x", x = project(x)))

*(x::Sym, y::Sym) =  Sym(pyeval("x * y", x = project(x), y = project(y)))
*(x::Sym, y::Real) = Sym(pyeval("x * y", x = project(x), y = project(y)))
*(x::Real, y::Sym) = Sym(pyeval("x * y", x = project(x), y = project(y)))


/(x::Sym, y::Sym) =  Sym(pyeval("x / y", x = project(x), y = project(y)))
/(x::Sym, y::Real) = Sym(pyeval("x / y", x = project(x), y = project(y)))
/(x::Real, y::Sym) = Sym(pyeval("x / y", x = project(x), y = project(y)))

^(x::Sym, y::Sym) =     Sym(pyeval("x ** y", x = project(x), y = project(y)))
^(x::Sym, y::Integer) = Sym(pyeval("x ** y", x = project(x), y = project(y)))
^(x::Sym, y::Real) =    Sym(pyeval("x ** y", x = project(x), y = project(y)))
^(x::Real, y::Sym) =    Sym(pyeval("x ** y", x = project(x), y = project(y)))



## Math functions       
for fn in (:sin, :cos, :tan, :sinh, :cosh, :tanh, :asin, :acos, :atan,
           :asinh, :acosh, :atanh, :sec, :csc, :cot, :asec, :acsc, :acot, 
           :sech, :csch, :coth, :asech, :acsch, :acoth, :sinc, :cosc, 
           :cosd, :cotd, :cscd, :secd, :sind, :tand,
           :acosd, :acotd, :acscd, :asecd, :asind, :atand, :atan2,
           :radians2degrees, :degrees2radians,
           :log, :log2, :log10, :log1p, :exponent, :exp, :exp2, :expm1,
           :cbrt, :sqrt, :square, :erf, :erfc, :erfcx, :erfi, :dawson,
           :ceil, :floor, :trunc, :round, :significand, 
           )


    meth = string(fn)
    @eval ($fn)(x::Sym) = Sym( sympy[symbol($meth)](project(x)) )
end

## add
abs(x::Sym) = Sym(sympy[:Abs](project(x)))

## Make a function argument, but munge arguments from Sym -> PyObject class
SymFunction(nm::Union(Symbol, String)) = (args...) -> Sym(sympy[:Function](nm)(project(args)...))

## Some sympy function interfaces

## SymPy's n. truncates a value at a certain point, makes numeric if not.
## Not quite what we want, n(pi, 3000) is not correct
n(x::Sym, args...) = call_meth(x, :n, args...)

subs(ex::Sym, x::Sym, arg) = call_meth(ex, :subs, x, arg)
subs(exs::Array{Sym}, x::Sym, arg) = [subs(ex, x, arg) for ex in exs]

## This is *experimental* syntax to lessen the typing of subs
## THis would work  ex | (x == 2) --> subs(ex, x, 2)
=={T <: Union(Real, Complex)}(x::Sym, y::T) = (ex) -> subs(ex, x, y)


for meth in (:expand, :together, :apart,
             :limit, :diff,
             :series, :integrate)
    meth_name = string(meth)
    @eval ($meth)(s::Sym, args...) = call_meth(s, symbol($meth_name), project(args)...)
end

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
Base.Array(::Type{Sym}, args...) = Base.Array(PyObject, args...)
Sym{N}(o::Array{PyObject,N}) = Sym(sympy[:Matrix](o))
Sym{T,N}(o::Array{T,N}) = Sym(convert(Array{PyObject,N}, o))
const SymMatrix = Sym

getindex(s::SymMatrix, i::Integer...) = Sym(pyeval("x[i]", x=s.x, i=tuple(([i...]-1)...)))
