
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
abs(x::Sym) = call_meth(:Abs, x)
abs(a::Array{Sym}) = map(abs, a)

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
             :primitive, :gcd, :lcm, :sqf, :resultant, :cancel,
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

## solve

## Is this a good idea? I want to be able to solve equations with
## solve(x^2 +x == x, x)
==(x::Sym, y::Sym) = solve(x - y)


## solve. Returns array of PyObjects
## Trying to return an array of Sym objects printed funny!
function solve(ex::Sym, x::Sym, args...)
    ans = sympy.solve(project(ex), project(x), project(args)...)
    Sym[u for u in ans]
end
function solve(ex::Sym)
    ans = sympy.solve(project(ex))
    Sym[u for u in ans]
end
function solve(exs::Vector{Sym}, xs::Vector{Sym})
    sympy[:solve](map(project, exs), map(project, xs)) #  dictionary with keys, values as PyObjects
end

## dsolve
## Make a function argument, but munge arguments from Sym -> PyObject class
SymFunction(nm::Union(Symbol, String)) = (args...) -> Sym(sympy[:Function](nm)(project(args)...))


##  A little trickier to use
## f = SymFunction("f")
## x = Sym("x")
## dsolve(diff(f(x), x) + f(x), f(x)) ## solve f'(x) + f(x) = 0
## dsolve(diff(f(x), x, x) + f(x), f(x)) ## solve f''(x) + f(x) = 0
dsolve(ex::Sym, fx::Sym) = call_meth(:dsolve, ex, fx)
