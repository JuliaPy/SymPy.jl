## these are differently named than SymPy or missing or ...


Base.abs(x::SymbolicObject) = sympy.Abs(x)
Base.abs2(x::SymbolicObject) = abs(x)^2
Base.max(x::Sym, a) = sympy.Max(x, a)
Base.min(x::Sym, a) = sympy.Min(x, a)


Base.cbrt(x::Sym) = x^(1//3)
Base.ceil(x::Sym) = ceiling(x)

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
limit(f::Function, x::Sym, c;kwargs...) = sympy.limit(f(x), x, c;kwargs...)
limit(f::Function, c;kwargs...) = limit(f, sympy.Dummy("x"), c;kwargs...)

## This is  type piracy  and a bad   idea
function Base.diff(f::Function, n::Int=1)
    x = sympy.Dummy("x")
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
    x = sympy.Dummy("x")
    sympy.integrate(f(x), (x, a, b))
end
function integrate(f::Function)
    x = sympy.Dummy("x")
    sympy.integrate(f(x), x)
end


## Add interfaces for solve, nonlinsolve when vector of equations passed in
solve(V::Vector{T}, args...; kwargs...) where {T <: SymbolicObject} =
    sympy.solve(V, args...; kwargs...)

"""
    nonlinsolve

Note: if passing variables in use a tuple (e.g., `(x,y)`) and *not* a vector (e.g., `[x,y]`).
"""
nonlinsolve(V::AbstractArray{T,N}, args...; kwargs...) where {T <: SymbolicObject, N} =
    sympy.nonlinsolve(V, args...; kwargs...)

linsolve(V::AbstractArray{T,N}, args...; kwargs...) where {T <: SymbolicObject, N} =
    sympy.linsolve(V, args...; kwargs...)
linsolve(Ts::Tuple, args...; kwargs...) where {T <: SymbolicObject} =
    sympy.linsolve(Ts, args...; kwargs...)


## dsolve allowing initial condiation to be specified

"""
   dsolve(eqn, var, args..,; ics=nothing, kwargs...)

Call `sympy.dsolve` with possible difference for initial condition specification.

For problems with an initial condition, the `ics` argument may be specified. This is *different* from the `ics` argument of `sympy.dsolve`. (Call directly if that is preferred.)

Here `ics` allows the specification of a term like `f(x0) = y0` as a tuple `(f, x0, y0)`. Similarly, a term like `f''(x0)=y0` is specified through `(f'', x0, y0)`. If more than one initial condition is needed, a tuple of tuples is used, as in `((f,x0,y0), (f',x0,z0))`.


Example:

```
x = Sym("x")
y = SymFunction("y")
eqn = y''(x) - y(x) - exp(x)
dsolve(eqn, y(x), ics=((y,0,1), (y, 1, 1//2)))
```
"""
function dsolve(eqn::Sym, args...; ics=nothing, kwargs...)
    if isa(ics, Nothing)
        sympy.dsolve(eqn, args...; kwargs...)
    else
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
        ## if more than one a tuple of tuples
        if eltype(ics) <: Tuple
            _dsolve(eqn, var, ics; kwargs...)
        else
            _dsolve(eqn, var, (ics,); kwargs...)
        end
    end
end

function _dsolve(eqn::Sym, var::Sym, ics; kwargs...)
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

rhs(x::SymbolicObject) = pycall_hasproperty(x, :rhs) ? x.rhs : x
lhs(x::SymbolicObject) = pycall_hasproperty(x, :lhs) ? x.lhs : x

export dsolve, rhs, lhs

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

# For System Of Ordinary Differential Equations
# may need to collect return values
dsolve(eqs::Union{Array, Tuple}, args...; kwargs...) = sympy.dsolve(eqs, args...; kwargs...)
