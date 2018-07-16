## Imported math functions

## need to import these
math_sympy_methods_base = (:sin, :cos, :tan, :sinh, :cosh, :tanh, :asin, :acos, :atan,
                           :asinh, :acosh, :atanh, :sec, :csc, :cot, :asec, :acsc, :acot,
                           :sech, :csch,
                           :coth, :acoth,
                           :log2, :log10, :log1p, :exponent, :exp, :exp2, :expm1,
                           :sqrt, :dawson,
                           :ceiling, :floor, 
                           :factorial,
                           :gcd, :lcm,
                           :isqrt
)

math_object_methods_base = (:round,
                            )


## Export SymPy math functions and vectorize them
math_sympy_methods = (:radians2degrees, :degrees2radians,
                      :factorial2,
                      )


# hypot and atan2
hypot(x::Sym, y::Number) = sqrt(x^2 + y^2)
atan2(y::Sym, x::Number) = sympy_meth(:atan2, y, x)

## Log function handles arguments differently
log(x::Sym) = sympy_meth(:log, x)
log(b::Sym, x::Sym) = sympy_meth(:log, x, b)

### Trigonometry
Base.rad2deg(x::Sym) = radians2degrees(x)
Base.deg2rad(x::Sym) = degrees2radians(x)

## degree functions
for fn in (:cosd, :cotd, :cscd, :secd, :sind, :tand,
          :acosd, :acotd, :acscd, :asecd, :asind, :atand)

    rad_fn = string(fn)[1:end-1]
    @eval ($fn)(x::Sym) = sympy[Symbol($rad_fn)](x * Sym(sympy["pi"])/180)
end

for fn in (:cospi, :sinpi)
    rad_fn = string(fn)[1:end-2]
    @eval ($fn)(x::Sym) = sympy[Symbol($rad_fn)](x * Sym(sympy["pi"]))
end

## :asech, :acsch, :sinc, :cosc,
## These fail, so define from definitions
## http://mathworld.wolfram.com/InverseHyperbolicSecant.html
asech(z::Sym) = log(sqrt(1/z-1)*sqrt(1/z+1) + 1/z)
## http://mathworld.wolfram.com/InverseHyperbolicCosecant.html
acsch(z::Sym) = log(sqrt(1+1/z^2) + 1/z)
# Julia's sinc is defined to be zero at x=1
sinc(x::Sym) = piecewise((Sym(1), Eq(x, 0)), (sin(PI*x)/(PI*x), Gt(abs(x), 0)))
cosc(x::Sym) = diff(sinc(x))

# deprecate these when v0.4 support dropped in favor of `asech.(...)` form
asech(as::AbstractArray{Sym}) = map(asech, as)
acsch(as::AbstractArray{Sym}) = map(acsch, as)


## in Julia, not SymPy
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
Base.abs2(x::Sym) = re(x*conj(x))
Base.copysign(x::Sym, y::Sym) = abs(x)*sign(y)
Base.signbit(x::Sym) = x < 0
Base.flipsign(x::Sym, y) = signbit(y) ? -x : x
Base.real(x::Sym) = sympy_meth(:re, x)
Base.imag(x::Sym) = sympy_meth(:im, x)
Base.eps(::Type{Sym}) = zero(Sym)


## use SymPy Names here...
## XXX This pattern may be of general usage. XXX
## It allows fn(Any, Sym) or f(Sym, Any) to match

import Base: min, max
min(x::Sym, a) = sympy_meth(:Min, x, a)
VERSION < v"0.7.0-" && (min(a, x::Union{SA, Real}) where {SA <: Sym} = min(x,a))

max(x::Sym, a) = sympy_meth(:Max, x, a)
VERSION < v"0.7.0-" && (max(a, x::Union{SA, Real}) where {SA <: Sym} = max(x,a))

# SymPy names
Min(ex::Sym, ex1::Sym) = sympy_meth(:Min, ex, ex1)
Max(ex::Sym, ex1::Sym) = sympy_meth(:Max, ex, ex1)




## Calculus functions

## bring over for univariate function calls as well (not just expressions)

"""

Compute a symbolic limit lim_{x->c+} f(x)

By default, *right* limits are returned. The keyword argument `dir="-"` needs to be
specified for *left* limits.

The function `f(x)` can be expressed as a symbolic expression, or a univariate function

The values `x` and `c` can be expressed by two arguments `x, c` or a pair `x=>c`.

```
@vars x y
ex = 1/(x^2 - y^2)
limit(ex, x=>3)
limit(ex, x, 3)
limit(ex, x=>3, y=>2)  # first x then y not (x,y) -> (3,2)

fn(x) = x^x
limit(fn, 0)    # symbol not needed
```
"""
limit(ex::Sym, args...; kwargs...) = sympy_meth(:limit, ex, args...; kwargs...)
limit(ex::Sym, d::Pair; kwargs...) = limit(ex, d.first, d.second;kwargs...)
limit(ex::Sym, ds::Pair...) = reduce(limit, ex, ds)
limit(f::Function, c::Number=0; kwargs...) = (z = (symbols(gensym())); limit(f(z),z=>c;kwargs...))

export limit


function Base.diff(ex::Sym, args...; kwargs...)
    if funcname(ex) in map(string, relational_sympy_values)
        Eq(diff(lhs(ex), args...; kwargs...), diff(rhs(ex), args...; kwargs...))
    else
        sympy_meth(:diff, ex, args...; kwargs...)
    end
end

##


## diff for matrix doesn't handle vectors well, so we vectorize here
diff(exs::AbstractVector{Sym}, args...; kwargs...) = map(ex -> diff(ex, args...;kwargs...), exs)
diff(exs::AbstractMatrix{Sym}, args...; kwargs...) = map(ex -> diff(ex, args...;kwargs...), exs)

## find symbolic derivatives from a function
function diff(f::Function, k::Int=1; kwargs...)
    x = Sym("x")
    diff(f(x), x, k; kwargs...)
end

# set up derivative, call doit to implement
Derivative(ex::Sym, args...) = sympy_meth(:Derivative, ex, args...)
export(Derivative)
#### Piecewise functions

"""

Create a piecewise defined function.

To create conditions on the variable, the functions `Lt`, `Le`, `Eq`, `Ge`, and `Gt` can be used. For infix notation,
unicode operators can be used: `\\ll<tab>`, `\\leqq<tab>`, `\\Equal<tab>`, `\\geqq<tab>`, and `\\gg<tab>` (but *not* `\\ge<tab>` or `\\le<tab>`).

To combine terms, the unicode `\\vee<tab>` (for "or"), `\\wedge<tab>` (for "and") can be used


Examples:
```
x,a = symbols("x,a")
p = piecewise((1, x ≪ 1), (2, (Lt(1,x)) ∨ Lt(x,2)), (3, x ≫ 2)) ## using ∨ and ∧ for & and or
subs(p, x, 2) ## 2
p = piecewise((1, Lt(x, a)), (2, Ge(x,a)))  # same as piecewise((1,  x ≪ a), (2, x ≧ a))
subs(p, x, a - 1)
```

[Note: there is also an alias `Piecewise` for copy-n-pasting from python code, but despite the capital letter, this is not a constructor for a type.]
"""
function piecewise(args...)
    sympy_meth(:Piecewise, args...)
end
const Piecewise = piecewise

piecewise_fold(ex::Sym) = sympy_meth(:piecewise_fold, ex)

VERSION < v"0.7.0-" && (Base.ifelse(ex::Sym, a, b) = piecewise((a, ex), (b, true)))

"""
Indicator expression: (Either `\\Chi[tab](x,a,b)` or `Indicator(x,a,b)`)

`Χ(x, a, b)` is `1` on `[a,b]` and 0 otherwise.


This is not a function taking `x`, but a symbolic expression of `x`.

"""
Χ(x::Sym, a=-oo, b=oo) = piecewise((1, Gt(x, a) ∧ Le(x, b)), (0,true))
Indicator(x::Sym, a=-oo, b=oo) = Χ(x, a, b)

import Base: &, |
(&)(a::Bool, b::Sym) = a & (b == SympyTRUE)
(|)(a::Bool, b::Sym) = a | (b == SympyTRUE)

export Indicator, Χ



### generic programming interface

## Handle ininf, and isnan by coercion to float
Base.isfinite(x::Sym) = isfinite(convert(Float64, x))
Base.isinf(x::Sym) = try isinf(convert(Float64, x)) catch e false end
Base.isnan(x::Sym) = try isnan(convert(Float64, x)) catch e false end

## we rename sympy.div -> polydiv
Base.div(x::Sym, y::SymOrNumber) = convert(Sym, sympy["floor"](x/convert(Sym,y)))
Base.rem(x::Sym, y::SymOrNumber) = x-Sym(y)*Sym(sympy["floor"](x/y))

## zero and one (zeros?)
Base.zero(x::Sym) = Sym(0)
Base.zero(::Type{Sym}) = Sym(0)

Base.one(x::Sym) = Sym(1)
Base.one(::Type{Sym}) = Sym(1)

## useful at times
Base.typemax(::Type{Sym}) = oo
Base.typemin(::Type{Sym}) = -oo


##################################################
## special numbers are initialized after compilation
if isdefined(PyCall,:PyNULL)
    pynull() = PyCall.PyNULL()
else
    pynull() = PyCall.PyObject()
end
global PI = Sym(pynull())
global E = Sym(pynull())
global IM = Sym(pynull())
global oo = Sym(pynull())


if isdefined(Base, :MathConstants)
    Base.convert(::Type{Sym}, x::Irrational{:π}) = PI
    Base.convert(::Type{Sym}, x::Irrational{:e}) = E
    Base.convert(::Type{Sym}, x::Irrational{:γ}) = Sym(sympy["EulerGamma"])
    Base.convert(::Type{Sym}, x::Irrational{:catalan}) = Sym(sympy["Catalan"])
    Base.convert(::Type{Sym}, x::Irrational{:φ}) = (1 + Sym(5)^(1//2))/2
else
    Base.convert(::Type{Sym}, x::Irrational{:π}) = PI
    Base.convert(::Type{Sym}, x::Irrational{:e}) = E
    Base.convert(::Type{Sym}, x::Irrational{:γ}) = Sym(sympy["EulerGamma"])
    Base.convert(::Type{Sym}, x::Irrational{:catalan}) = Sym(sympy["Catalan"])
    Base.convert(::Type{Sym}, x::Irrational{:φ}) = (1 + Sym(5)^(1//2))/2
end
function init_math()
    "PI is a symbolic  π. Using `julia`'s `pi` will give round off errors."
    copy!(PI.x,  sympy["pi"])

    "E is a symbolic  `e`. Using `julia`'s `e` will give round off errors."
    copy!(E.x, Sym(sympy["exp"](1)).x)

    "IM is a symbolic `im`"
    copy!(IM.x, sympy["I"])

    "oo is a symbolic infinity. Example: `integrate(exp(-x), x, 0, oo)`."
    copy!(oo.x, sympy["oo"])
end
