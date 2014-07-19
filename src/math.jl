
## Math functions       
for fn in (:sin, :cos, :tan, :sinh, :cosh, :tanh, :asin, :acos, :atan,
           :asinh, :acosh, :atanh, :sec, :csc, :cot, :asec, :acsc, :acot, 
           :sech, :csch, :coth, :asech, :acsch, :acoth, :sinc, :cosc, 
           :atan2,
           :radians2degrees, :degrees2radians,
           :log, :log2, :log10, :log1p, :exponent, :exp, :exp2, :expm1,
           :sqrt, :square, :erf, :erfc, :erfcx, :erfi, :dawson,
           :ceiling, :floor, :trunc, :round, :significand
           )

    
    meth = string(fn)
    @eval ($fn)(x::Sym) = sympy.(symbol($meth))(project(x))
    @eval ($fn)(a::Array{Sym}) = map($fn, a)
end

## in julia, not SymPy
cbrt(x::Sym) = PyCall.pyeval("x ** (1/3)", x=project(x)) 
Base.ceil(x::Sym) = ceiling(x)

   
for fn in (:cosd, :cotd, :cscd, :secd, :sind, :tand,
          :acosd, :acotd, :acscd, :asecd, :asind, :atand)

    rad_fn = string(fn)[1:end-1]
    @eval ($fn)(x::Sym) = sympy.(symbol($rad_fn))(project(x * Sym(sympy.pi)/180))
    @eval ($fn)(a::Array{Sym}) = map($fn, a)
end
                                           


## add
abs(x::Sym) = sympy_meth(:Abs, x)
abs(a::Array{Sym}) = map(abs, a)

Base.isless(a::Real, b::Sym) = isless(a, float(b))
Base.isless(a::Sym, b::Real) = isless(b, a)
Base.isfinite(x::Sym) = isfinite(float(x))

## Some sympy function interfaces

## subs
function subs{T <: SymbolicObject, S <: SymbolicObject}(ex::T, x::S, arg)
    object_meth(ex, :subs, x, arg)
end
subs{T <: SymbolicObject, S <: SymbolicObject}(exs::Array{T}, x::S, arg) = map(ex->subs(ex, x, arg), exs)
## curried version to use with |>
subs(x::SymbolicObject, y) = ex -> subs(ex, x, y)
## convenience method to use symbol
subs{T <:SymbolicObject}(ex::T, x::Symbol, arg) = subs(ex, Sym(x), arg)

Base.replace(ex::SymbolicObject, x::SymbolicObject, y) = subs(ex, x, y)
## curried version to use through |> as in
## ex |> replace(x, 2)
Base.replace(x::SymbolicObject, y) = ex -> subs(ex, x, y)




function !={T <: Real}(x::Sym, y::T) 
    try 
        x = float(x)
        x != y
    catch
        true
    end
end
function !={T <: Complex}(x::Sym, y::T) 
    try 
        x = complex(x)
        x != y
    catch
        true
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

## bring over for function calls (not expressions)
## returns a symbolic expression
## limit(f, c) or limit(f,c,dir="-") or limit(f, c, dir="-")
function limit(f::Function, c::Number=0; kwargs...)
    x = Sym("x")
    ## catch some values...
    if abs(c) == Inf
        c = sign(c) * oo
    elseif c == Base.pi
        c = Sym(sympy.pi)
    elseif c == Base.e
        c = Sym(sympy.exp(1))
    end
    limit(f(x), x, c; kwargs...)
end

## find symbolic derivatives from a function
function diff(f::Function, k::Int=1)
    x = Sym("x")
    diff(f(x), x, k)
end
    


## different conversions
fraction(args...; kwargs...) = sympy.fraction(project(args)...; kwargs...) |> os -> map(u -> convert(Sym, u), os)




## special numbers
I = Sym(sympy.I)
oo = Sym(sympy.oo)

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
    @eval ($meth)(ex::Sym, args...) = Sym(sympy.(symbol($meth_name))(project(ex), project(args)...))
    eval(Expr(:export, meth))
end

## solve

## DEPRECATED
## Is this a good idea? I want to be able to solve equations with
## solve(x^2 +x == x, x)
##==(x::Sym, y::Sym) = solve(x - y)

## Experimental! Not sure these are such a good idea ...
## but used with piecewise


Base.&(x::Sym, y::Sym) = PyCall.pyeval("x & y", x=project(x), y=project(y))
Base.|(x::Sym, y::Sym) = PyCall.pyeval("x | y", x=project(x), y=project(y))
!(x::Sym)         =      PyCall.pyeval("~x",    x=project(x))
## version 0.3 forward
if VERSION >= v"0.3.0-rc1+82"
    ## use ∨, ∧, ¬ for |,&,! (\vee<tab>, \wedge<tab>, \neg<tab>)
    ∨(x::Sym, y::Sym) = x | y
    ∧(x::Sym, y::Sym) = x & y
    ¬(x::Sym) = !x
end


<(x::Sym,  y::Number) = PyCall.pyeval("x < y", x=project(x), y=project(y))
<=(x::Sym, y::Number) = PyCall.pyeval("x <= y", x=project(x), y=project(y))
>=(x::Sym, y::Number) = PyCall.pyeval("x >= y", x=project(x), y=project(y))
>(x::Sym, y::Number)  = PyCall.pyeval("x > y", x=project(x), y=project(y))
## hacky, but == is something else to SymPy
==(x::Sym, y::Number) = (x <= y) & (x >= y)

<(x::Number, y::Sym)  = y > x
<=(x::Number, y::Sym) = y >= x
>=(x::Number, y::Sym) = y <= x
>(x::Number, y::Sym)  = y < x




## ==(x::Sym, y) = sympy_meth(:Eq, x, y, args...; kwargs...)
##isequal(x::Sym, y::Sym, args...; kwargs...) = sympy_meth(:Eq, x, y, args...; kwargs...)



## use equality if a python level.
#Base.isequal(x::Sym, y::Sym) = isequal(x.x, y.x)
==(x::Sym, y::Sym) = x.x == y.x
# ==(x::Sym, y::Number) = x == convert(Sym, y)
# ==(x::Number, y::Sym) = convert(Sym,x) == y

Base.isinf(x::Sym) = try isinf(float(x)) catch e false end
Base.isnan(x::Sym) = try isnan(float(x)) catch e false end


Base.div(x::Sym, y) = convert(Sym, sympy.floor(project(x/convert(Sym,y))))

Base.rem(x::Sym, y) = x-Sym(y)*Sym(sympy.floor(project(x/y)))

Base.zero(x::Sym) = oftype(Sym, 0)
Base.zero{T<:Sym}(::Type{T}) = oftype(T,0)

Base.one(x::Sym) = oftype(Sym, 1)
Base.one{T<:Sym}(::Type{T}) = oftype(T, 1)

## solve. Returns array of PyObjects
## Trying to return an array of Sym objects printed funny!
function solve(ex::Sym, args...; kwargs...)
    ans = sympy.solve(project(ex), map(project, args)...; kwargs...)
    Sym[u for u in ans]
end


function solve(exs::Vector{Sym}, xs::Vector{Sym}, args...; kwargs...)
    ans = sympy.solve(map(project, exs), map(project, xs), args...; kwargs...) #  dictionary with keys, values as PyObjects
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
SymFunction(nm::Union(Symbol, String)) = (args...) -> Sym(sympy.Function(nm)(project(args)...))


##  A little trickier to use
## f = SymFunction("f")
## x = Sym("x")
## dsolve(diff(f(x), x) + f(x), f(x)) ## solve f'(x) + f(x) = 0
## dsolve(diff(f(x), x, x) + f(x), f(x)) ## solve f''(x) + f(x) = 0
dsolve(ex::Sym, fx::Sym) = sympy_meth(:dsolve, ex, fx)


## Piecewise
## pass in tuples, eg.
## p = piecewise((1, x < 1), (2, (1 <= x) ∨ (x <= 2)), (3, x > 2)) ## using ∨ and ∧ for & and or
function piecewise(args...)
    args = [map(project, x) for x in args]
    sympy.Piecewise(args...)
end
