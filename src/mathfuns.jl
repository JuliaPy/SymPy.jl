## these are differently named than SymPy or missing or ...


Base.abs(x::SymbolicObject) = sympy.Abs(x)
Base.abs2(x::SymbolicObject) = x * conj(x)
Base.max(x::Sym, a) = sympy.Max(x, a)
Base.min(x::Sym, a) = sympy.Min(x, a)


Base.cbrt(x::Sym) = x^(1//3)
Base.ceil(x::Sym) = sympy.ceiling(x)

## Trig
Base.asech(z::Sym) = log(sqrt(1/z-1)*sqrt(1/z+1) + 1/z)
Base.acsch(z::Sym) = log(sqrt(1+1/z^2) + 1/z) ## http://mathworld.wolfram.com/InverseHyperbolicCosecant.html
Base.atan(y::Sym, x) = sympy.atan2(y,x)


Base.sinc(x::Sym) = iszero(x) ? one(x) : sin(PI*x)/(PI*x)
cosc(x::Sym) = diff(sinc(x))

Base.sincos(x::Sym) = (sin(x), cos(x))
Base.sinpi(x::Sym) = sympy.sin(x*PI)
Base.cospi(x::Sym) = sympy.cos(x*PI)
degree_variants = (:sind, :cosd, :tand, :cotd, :secd, :cscd,
                   :asind, :acosd, :atand, :acotd, :asecd, :acscd)

for  methvar in degree_variants
    meth = Symbol(String(methvar)[1:end-1])
    @eval begin
        (Base.$methvar)(ex::SymbolicObject) = ($meth)((PI/180)*ex)
    end
end

Base.rad2deg(x::Sym) = (x * 180) / PI
Base.deg2rad(x::Sym) = (x * PI) / 180

Base.hypot(x::Sym, y::Number) = hypot(promote(x,y)...)
Base.hypot(xs::Sym...) = sqrt(sum(abs(xᵢ)^2 for xᵢ ∈ xs))

## exponential
Base.log1p(x::Sym) = sympy.log(1 + x)
Base.log(x::Sym) = sympy.log(x)
Base.log(b::Number, x::Sym) = sympy.log(x, b)
Base.log2(x::SymbolicObject) = log(2,x)
Base.log10(x::SymbolicObject) = log(10,x)




## calculus.
## use a pair for limit x=>0
limit(x::SymbolicObject, xc::Pair, args...;kwargs...) = limit(x, xc[1], xc[2], args...;kwargs...)
## allow a function
limit(f::Function, x::Sym, c;kwargs...) = limit(Sym(f(x)), x, c; kwargs...)
function  limit(f::Function, c;kwargs...)
    @vars  x
    limit(f, x, c; kwargs...)
end

## This is  type piracy  and a bad   idea
function Base.diff(f::Function, n::Integer=1)
    @vars x
    sympy.diff(f(x), x, n)
end

## integrate(ex,a,b)
function integrate(ex::SymbolicObject, a::Number, b::Number)
    fs = free_symbols(ex)
    if length(fs) !== 1
        @warn "Need exactly on free symbol. Use `integrate(ex, (x, a, b))` instead"
        return
    end
    integrate(ex, (fs[1], a, b))
end
function integrate(f::Function, a::Number, b::Number)
    @vars x
    sympy.integrate(f(x), (x, a, b))
end
function integrate(f::Function)
    @syms  x
    sympy.integrate(f(x), x)
end


## Add interfaces for solve, nonlinsolve when vector of equations passed in

## An alternative to Eq(lhs, rhs) following Symbolics.jl
"""
    lhs ~ rhs

Specify an equation.

Alternative syntax to `Eq(lhs, rhs)` or `lhs ⩵ rhs` (`\\Equal[tab]`) following `Symbolics.jl`.
"""
Base.:~(lhs::Number, rhs::SymbolicObject) = Eq(lhs, rhs)
Base.:~(lhs::SymbolicObject, rhs::Number) = Eq(lhs, rhs)
Base.:~(lhs::SymbolicObject, rhs::SymbolicObject) = Eq(lhs, rhs)


"""
    solve

Use `solve` to solve algebraic equations.

Examples:

```julia
julia> using SymPy

julia> @syms x y a b c d
(x, y, a, b, c, d)

julia> solve(x^2 + 2x + 1, x) # [-1]
1-element Vector{Sym}:
 -1

julia> solve(x^2 + 2a*x + a^2, x) # [-a]
1-element Vector{Sym}:
 -a

julia> solve([a*x + b*y-3, c*x + b*y - 1], [x,y]) # Dict(y => (a - 3*c)/(b*(a - c)),x => 2/(a - c))
Dict{Any, Any} with 2 entries:
  y => (a - 3*c)/(a*b - b*c)
  x => 2/(a - c)

```

!!! note
    A very nice example using `solve` is a [blog](https://newptcai.github.io/euclidean-plane-geometry-with-julia.html) entry on [Napolean's theorem](https://en.wikipedia.org/wiki/Napoleon%27s_theorem) by Xing Shi Cai.
"""
solve() = ()


"""
    nonlinsolve

Note: if passing variables in use a tuple (e.g., `(x,y)`) and *not* a vector (e.g., `[x,y]`).
"""
nonlinsolve()


## dsolve allowing initial condiation to be specified

"""
   dsolve(eqn, var, args..,; ics=nothing, kwargs...)

Call `sympy.dsolve`.

The initial conditions are specified with a dictionary.

Example:

```jldoctest dsolve
julia> using SymPy

julia> @syms α, x, f(), g()
(α, x, f, g)

julia> ∂ = Differential(x)
Differential(x)

julia> eqn = ∂(f(x)) ~ α * x
d
──(f(x)) = x⋅α
dx
```

```julia
julia> dsolve(eqn)
             2
            x ⋅α
f(x) = C₁ + ────
             2
```

```jldoctest dsolve
julia> dsolve(eqn(α=>2); ics=Dict(f(0)=>1)) |> print # fill in parameter, initial condition
Eq(f(x), x^2 + 1)

julia> eqn = ∂(∂(f(x))) ~ -f(x); print(eqn)
Eq(Derivative(f(x), (x, 2)), -f(x))

julia> dsolve(eqn)
f(x) = C₁⋅sin(x) + C₂⋅cos(x)

julia> dsolve(eqn; ics = Dict(f(0)=>1, ∂(f)(0) => -1))
f(x) = -sin(x) + cos(x)

julia> eqn = ∂(∂(f(x))) - f(x) - exp(x);

julia> dsolve(eqn, ics=Dict(f(0) => 1, f(1) => Sym(1//2))) |> print # not just 1//2
Eq(f(x), (x/2 + (-exp(2) - 2 + E)/(-2 + 2*exp(2)))*exp(x) + (-E + 3*exp(2))*exp(-x)/(-2 + 2*exp(2)))
```

Systems

```jldoctest dsolve
julia> @syms x() y() t g
(x, y, t, g)

julia> ∂ = Differential(t)
Differential(t)

julia> eqns = (∂(x(t)) ~ y(t), ∂(y(t)) ~ x(t))
(Eq(Derivative(x(t), t), y(t)), Eq(Derivative(y(t), t), x(t)))

julia> dsolve(eqns)
2-element Vector{Sym}:
 Eq(x(t), -C1*exp(-t) + C2*exp(t))
  Eq(y(t), C1*exp(-t) + C2*exp(t))

julia> dsolve(eqns, ics = Dict(x(0) => 1, y(0) => 2))
2-element Vector{Sym}:
 Eq(x(t), 3*exp(t)/2 - exp(-t)/2)
 Eq(y(t), 3*exp(t)/2 + exp(-t)/2)

julia> eqns = (∂(∂(x(t))) ~ 0, ∂(∂(y(t))) ~ -g)
(Eq(Derivative(x(t), (t, 2)), 0), Eq(Derivative(y(t), (t, 2)), -g))

julia> dsolve(eqns)  # can't solve for initial conditions though! (NotAlgebraic)
2-element Vector{Sym}:
           x(t) = C₁ + C₂⋅t
 Eq(y(t), C3 + C4*t - g*t^2/2)

julia> @syms t x() y()
(t, x, y)

julia> eq = (∂(x)(t) ~ x(t)*y(t)*sin(t), ∂(y)(t) ~ y(t)^2 * sin(t))
(Eq(Derivative(x(t), t), x(t)*y(t)*sin(t)), Eq(Derivative(y(t), t), y(t)^2*sin(t)))
```

```julia
julia> dsolve(eq)  # returns a set to be `collect`ed:
PyObject {Eq(x(t), -exp(C1)/(C2*exp(C1) - cos(t))), Eq(y(t), -1/(C1 - cos(t)))}
```

```julia
julia> dsolve(eq) |> collect
2-element Vector{Any}:
 Eq(x(t), -exp(C1)/(C2*exp(C1) - cos(t)))
               Eq(y(t), -1/(C1 - cos(t)))
```

"""
function dsolve(eqn, args...;
                ics::Union{Nothing, AbstractDict, Tuple}=nothing,
                kwargs...)
    if isa(ics, Tuple) # legacy
        _dsolve(eqn, args...; ics=ics, kwargs...)
    else
        sympy.dsolve(eqn, args...; ics=ics, kwargs...)
    end
end

rhs(x::SymbolicObject) = pycall_hasproperty(x, :rhs) ? x.rhs : x
lhs(x::SymbolicObject) = pycall_hasproperty(x, :lhs) ? x.lhs : x


export dsolve, rhs, lhs

## ----

## Add methods for "solve functions"
for meth ∈ (:solve, :linsolve, :nonlinsolve, :nsolve, :dsolve)
    m = Symbol(meth)
    @eval begin
        ($meth)(V::AbstractArray{T,N}, args...; kwargs...) where {T <: SymbolicObject, N} = sympy.$meth(V, args...; kwargs...)
        ($meth)(Ts::NTuple{N,T}, args...; kwargs...) where {N, T <: SymbolicObject} =
            sympy.$meth(Ts, args...; kwargs...)
        ($meth)(Ts::Tuple, args...; kwargs...) =
            sympy.$meth(Ts, args...; kwargs...)
    end
end



## ---- deprecate ----

## used with ics=(u,0,1) style
function _dsolve(eqn::Sym, args...; ics=nothing, kwargs...)

    Base.depwarn("Use of tuple(s), `(u, x₀, u₀)`, to specify initial conditions is deprecated. Use a dictionary: `ics=Dict(u(x₀) => u₀)`.", :_dsolve)

    if isempty(args)
        var = first(free_symbols(eqn))
    else
        var = first(args)
    end
    # var might be f(x) or x, we want `x`
    if Introspection.classname(var) != "Symbol"
        var = first(var.args)
    end
    ## if we have one initial condition, can be passed in a (u,x0,y0) *or* ((u,x0,y0),)
    ## if more than oneq a tuple of tuples
    if eltype(ics) <: Tuple
        __dsolve(eqn, var, ics; kwargs...)
    else
        __dsolve(eqn, var, (ics,); kwargs...)
        end
end

function __dsolve(eqn::Sym, var::Sym, ics; kwargs...)
    if length(ics) == 0
        throw(ArgumentError("""Some initial value specification is needed.
Specifying the function, as in `dsolve(ex, f(x))`, is deprecated.
Use `sympy.dsolve(ex, f(x); kwargs...)` directly for that underlying interface.
"""))
    end

    out = sympy.dsolve(eqn; kwargs...)
    ord = sympy.ode_order(eqn, var)

    ## `out` may be an array of solutions. If so we do each one.
    ## we want to use an array for output only if needed
    if !isa(out, Array)
        return _solve_ivp(out, var, ics,ord)
    else
        output = Sym[]
        for o in out
            a = _solve_ivp(o, var, ics,ord)
            a != nothing && push!(output, a)
        end
        return length(output) == 1 ? output[1] : output
    end
end

## Helper.
## out is an equation in var with constants. Args are intial conditions
## Return `nothing` if initial condition is not satisfied (found by `solve`)
function _solve_ivp(out, var, args, o)

    eqns = Sym[(diff(out.rhs(), var, f.n))(var=>x0) - y0 for (f, x0, y0) in args]
    sols = solve(eqns, Sym["C$i" for i in 1:o], dict=true)
    if length(sols) == 0
       return nothing
    end

    ## massage output
    ## Might have more than one solution, though unlikely. But if we substitute a variable
    ## for y0 we will get an array back from solve which may have length 1.
    if isa(sols, Array)
        if length(sols) == 1
            sols = sols[1]
        else
            return [out([Pair(k,v) for (k,v) in sol]...) for sol in sols]
        end
    end

    out([Pair(k,v) for (k,v) in sols]...)
end

## For System Of Ordinary Differential Equations
## may need to collect return values
# dsolve(eqs::Union{Array, Tuple}, args...; kwargs...) = sympy.dsolve(eqs, args...; kwargs...)
