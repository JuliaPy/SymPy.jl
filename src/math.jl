## Imported math functions
## make vectorized version while we are at it
for fn in (:sin, :cos, :tan, :sinh, :cosh, :tanh, :asin, :acos, :atan,
           :asinh, :acosh, :atanh, :sec, :csc, :cot, :asec, :acsc, :acot, 
           :sech, :csch, 
           :coth, :acoth, 
           :atan2,
           :radians2degrees, :degrees2radians,
           :log2, :log10, :log1p, :exponent, :exp, :exp2, :expm1,
           :sqrt, :square, :erf, :erfc, :erfcx, :erfi, :dawson,
           :ceiling, :floor, :trunc, :round, :significand,
           :factorial2,
           :airyai, :airybi
           )
    meth = string(fn)
    @eval ($fn)(x::Sym;kwargs...) = sympy_meth(symbol($meth), x; kwargs...)#sympy[symbol($meth)](project(x),[(k,project(v)) for (k,v) in kwargs]...)
    @eval ($fn)(a::Array{Sym}) = map($fn, a)
end


## Log function handles arguments differently
log(x::Sym) = sympy_meth(:log, x)
log(b::Sym, x::Sym) = sympy_meth(:log(x,b))

### Trigonometry

if VERSION >= v"0.4.0-dev"
    Base.rad2deg(x::Sym) = radians2degrees(x)
    Base.deg2rad(x::Sym) = degrees2radians(x)
end

## degree functions   
for fn in (:cosd, :cotd, :cscd, :secd, :sind, :tand,
          :acosd, :acotd, :acscd, :asecd, :asind, :atand)

    rad_fn = string(fn)[1:end-1]
    @eval ($fn)(x::Sym) = sympy[symbol($rad_fn)](project(x * Sym(sympy[:pi]/180)))
    @eval ($fn)(a::Array{Sym}) = map($fn, a)
end
                                           
for fn in (:cospi, :sinpi)
    rad_fn = string(fn)[1:end-2]
    @eval ($fn)(x::Sym) = sympy[symbol($rad_fn)](project(x * Sym(sympy[:pi])))
    @eval ($fn)(a::Array{Sym}) = map($fn, a)
end

## :asech, :acsch, :sinc, :cosc, 
## These fail, so define from definitions
## http://mathworld.wolfram.com/InverseHyperbolicSecant.html
asech(z::Sym) = log(sqrt(1/z-1)*sqrt(1/z+1) + 1/z)
asech(as::Array{Sym}) = map(asech, as)
## http://mathworld.wolfram.com/InverseHyperbolicCosecant.html
acsch(z::Sym) = log(sqrt(1+1/z^2) + 1/z)
acsch(as::Array{Sym}) = map(acsch, as)
sinc(x::Sym) = sin(Sym(PI*x))/(PI*x)
sinc(as::Array{Sym}) = map(sinc, as)
cosc(x::Sym) = diff(sinc(x))
cosc(as::Array{Sym}) = map(cosc, as)


## (x:Sym, ...) , export
sympy_math_methods = (:Prod,
                      :Ylm, 
                      :assoc_legendre, 
                      :chebyshevt
                      )
for meth in sympy_math_methods 
    meth_name = string(meth)
    @eval ($meth)(ex::Sym, args...; kwargs...) = sympy_meth(symbol($meth_name), ex, args...; kwargs...)
    eval(Expr(:export, meth))
end



## in julia, not SymPy
cbrt(x::Sym) = x^(1//3)
Base.ceil(x::Sym) = ceiling(x)
 
functions_sympy_methods = (
                           :arg,
                           :conjugate,
                           :re,
                           :sign
                           )


## map Abs->abs, Max->max, Min->min
abs(ex::Sym, args...; kwargs...) = sympy_meth(:Abs, ex, args...; kwargs...)
Base.real(x::Sym) = sympy_meth(:re, x)
Base.imag(x::Sym) = sympy_meth(:im, x)

## sign-related functions
abs(x::Sym) = sympy_meth(:Abs, x)
abs(a::Array{Sym}) = map(abs, a)
Base.abs2(x::Sym) = re(x*conj(x))
Base.copysign(x::Sym, y::Sym) = abs(x)*sign(y)

#minimum(ex::Sym,x::NAtype) = x
minimum(ex::Sym, args...; kwargs...) = sympy_meth(:Min, ex, args...; kwargs...)
#maximum(ex::Sym,x::NAtype) = x
maximum(ex::Sym, args...; kwargs...) = sympy_meth(:Max, ex, args...; kwargs...)



for meth in (:separate, :flatten, 
             :igcd, :ilcm,
             :sqf,
             :together, 
             :limit, 
             :Derivative
             )
    meth_name = string(meth)
    @eval ($meth)(ex::Sym, args...; kwargs...) = sympy_meth(symbol($meth_name), ex, args...; kwargs...)
    eval(Expr(:export, meth))
end

## Calculus functions
function Base.diff(ex::Sym, args...; kwargs...)
    if ex[:is_Equality]
        Eq(diff(lhs(ex), args...; kwargs...), diff(rhs(ex), args...; kwargs...))
    else
        sympy_meth(:diff, ex, args...; kwargs...)
    end
end
    



## diff for matrix doesn't handle vectors well, so we vectorize here
diff(exs::Array{Sym}, args...; kwargs...) = map(ex -> diff(ex, args...;kwargs...), exs)

## bring over for function calls (not expressions)
## returns a symbolic expression
## limit(f, c) or limit(f,c,dir="-") or limit(f, c, dir="-")
function limit(f::Function, c::Number=0; kwargs...)
    x = Sym("x")
    limit(f(x), x, c; kwargs...)
end

## find symbolic derivatives from a function
function diff(f::Function, k::Int=1; kwargs...)
    x = Sym("x")
    diff(f(x), x, k; kwargs...)
end
    



## Comparisons Real, Sym
Base.isless(a::Real, b::Sym) = isless(a, convert(Float64, b))
Base.isless(a::Sym, b::Real) = isless(b, a)
Base.isfinite(x::Sym) = isfinite(convert(Float64, x))



## Handle ininf, and isnan by coercion to float
Base.isinf(x::Sym) = try isinf(convert(Float64, x)) catch e false end
Base.isnan(x::Sym) = try isnan(convert(Float64, x)) catch e false end

## we rename sympy.div -> polydiv
Base.div(x::Sym, y::Union(Sym, Number)) = convert(Sym, sympy[:floor](project(x/convert(Sym,y))))
Base.rem(x::Sym, y::Union(Sym, Number)) = x-Sym(y)*Sym(sympy[:floor](project(x/y)))

## zero and one (zeros?)
Base.zero(x::Sym) = Sym(0)
Base.zero(::Type{Sym}) = Sym(0)

Base.one(x::Sym) = Sym(1)
Base.one(::Type{Sym}) = Sym(1)


#### Piecewise functions

"""

Create a piecewise defined function.

To create conditions on the variable, the functions `Lt`, `Le`, `Eq`, `Ge`, and `Gt` can be used. For infix notation, 
unicode operators can be used: `\ll<tab>`, `\le<tab>`, `\Equal<tab>`, `\ge<tab>`, and `\gg<tab>`.

To combine terms, the unicode `\vee<tab>` (for "or"), `\wedge<tab>` (for "and") can be used


Examples:
```
x,a = symbols("x,a")
p = piecewise((1, x ≪ 1), (2, (1 ≤ x) ∨ (x ≤ 2)), (3, x ≫ 2)) ## using ∨ and ∧ for & and or
subs(p, x, 2) ## 2
p = piecewise((1, Lt(x, a)), (2, Ge(x,a)))  # same as piecewise((1,  x ≪ a), (2, x ≥ a))
subs(p, x, a - 1)
```
"""                 
function piecewise(args...)
    args = [map(project, x) for x in args]
    sympy_meth(:Piecewise, args...)
end




## special numbers are initialized after compilation
function init_math()
    "PI is a symbolic  π. Using `julia`'s `pi` will give round off errors." 
    global const PI = sympy[:pi]

    "E is a symbolic  `e`. Using `julia`'s `e` will give round off errors." 
    global const E = sympy[:exp](1)
    
    "IM is a symbolic `im`" 
    global const IM = sympy[:I]

    "oo is a symbolic infinity. Example: `integrate(exp(-x), x, 0, oo)`." 
    global const oo = sympy[:oo]


    ## math constants
    Base.convert(::Type{Sym}, x::MathConst{:π}) = PI
    Base.convert(::Type{Sym}, x::MathConst{:e}) = E
    Base.convert(::Type{Sym}, x::MathConst{:γ}) = sympy[:EulerGamma]
    Base.convert(::Type{Sym}, x::MathConst{:catalan}) = sympy[:Catalan]
    Base.convert(::Type{Sym}, x::MathConst{:φ}) = (1 + Sym(5)^(1//2))/2
end
