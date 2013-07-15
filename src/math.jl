
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
    @eval ($fn)(x::Sym) = sympy[symbol($meth)](project(x))
    @eval ($fn)(a::Array{Sym}) = map($fn, a)
end


    


## add
abs(x::Sym) = sympy_meth(:Abs, x)
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

## evalf, n, N

for meth in (:n, :N,
             :separate, :flatten, 
             :igcd, :ilcm,
             :sqf,
             :together, 
             :limit, 
             :diff, :Derivative
             )
    meth_name = string(meth)
    @eval ($meth)(ex::Sym, args...; kwargs...) = sympy_meth(symbol($meth_name), ex, args...; kwargs...)
    eval(Expr(:export, meth))
end

## different conversions
fraction(args...; kwargs...) = sympy.fraction(project(args)...; kwargs...) | os -> map(u -> convert(Sym, u), os)




## special numbers
I = Sym(sympy[:I])
oo = Sym(sympy[:oo])

## Special functions
## Spherical harmonic



## functions which are methods of sympy, not a symbolic instance


sympy_math_methods = ( :Prod,
                      :Ylm, 
                      :gamma, :beta, # need import
                      :assoc_legendre, 
                      :chebyshevt, 
                      :legendre, 
                      :hermite
                      )
for meth in sympy_math_methods 
    meth_name = string(meth)
    @eval ($meth)(ex::Sym, args...) = Sym(sympy[symbol($meth_name)](project(ex), project(args)...))
    eval(Expr(:export, meth))
end

## solve

## Is this a good idea? I want to be able to solve equations with
## solve(x^2 +x == x, x)
==(x::Sym, y::Sym) = solve(x - y)


## solve. Returns array of PyObjects
## Trying to return an array of Sym objects printed funny!
function solve(ex::Sym, x::Sym, args...)
    ans = sympy.solve(project(ex), project(x), project(args)...)
    ans
    #Sym[u for u in ans]
end
function solve(ex::Sym)
    ans = sympy.solve(project(ex))
    ans
    #Sym[u for u in ans]
end


function solve(exs::Vector{Sym}, xs::Vector{Sym})
    ans = sympy[:solve](map(project, exs), map(project, xs)) #  dictionary with keys, values as PyObjects
    [string(k) => v for (k,v) in ans]
end

## Numeric solutions
nsolve(ex::Sym, x::Sym, x0::Number) = sympy.nsolve(project(ex), project(x), x0) |> float
nsolve(ex::Sym, x0::Number) =  sympy.nsolve(project(ex), x0) |> float
function nsolve{T <: Number}(ex::Vector{Sym}, x::Vector{Sym}, x0::Vector{T}; kwargs...)
    ans = sympy.nsolve(tuple(map(project,ex)...), tuple(map(project,x)...), tuple(x0...); kwargs...)
    ## ans is matrix object -- convert
    convert(Array{Sym}, sympy.Matrix(ans)) |> float
end
export nsolve

## dsolve
## Make a function argument, but munge arguments from Sym -> PyObject class
SymFunction(nm::Union(Symbol, String)) = (args...) -> Sym(sympy[:Function](nm)(project(args)...))


##  A little trickier to use
## f = SymFunction("f")
## x = Sym("x")
## dsolve(diff(f(x), x) + f(x), f(x)) ## solve f'(x) + f(x) = 0
## dsolve(diff(f(x), x, x) + f(x), f(x)) ## solve f''(x) + f(x) = 0
dsolve(ex::Sym, fx::Sym) = sympy_meth(:dsolve, ex, fx)
