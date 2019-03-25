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

sinc(x::Sym) = Piecewise((Sym(1), sympy.Eq(x, 0)),
                         (sin(PI*x)/(PI*x), sympy.Gt(abs(x), 0))
                         ) # Julia's sinc is defined to be zero at x=1
cosc(x::Sym) = diff(sinc(x))

Base.sincos(x::Sym) = (sin(x), cos(x))

Base.rad2deg(x::Sym) = (x * 180) / PI
Base.deg2rad(x::Sym) = (x * PI) / 180

## exponential
Base.log(b::Number, x::Sym) = sympy.log(x, b)






## calculus.
## use a pair for limit x=>0
limit(x::SymbolicObject, xc::Pair, args...;kwargs...) = limit(x, xc[1], xc[2], args...;kwargs...)
## allow a function
limit(f::Function, x::Sym, c;kwargs...) = limit(f(x), x, c;kwargs...)
limit(f::Function, c;kwargs...) = limit(f, symbols("x"), c;kwargs...)


function Base.diff(f::Function, n::Int=1)
    x = symbols("x")
    diff(f(x), x, n)
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
    x = symbols("x")
    integrate(f(x), (x, a, b))
end
function integrate(f::Function)
    x = symbols("x")
    integrate(f(x))
end


## Add interfaces for solve, nonlinsolve when vector of equations passed in
solve(V::Vector{T}, args...; kwargs...) where {T <: SymbolicObject} =
    sympy.solve(convert(SymMatrix, V), args...; kwargs...)

nonlinsolve(V::Vector{T}, args...; kwargs...) where {T <: SymbolicObject} =
    sympy.nonlinsolve(convert(SymMatrix, V), args...; kwargs...)

linsolve(V::Vector{T}, args...; kwargs...) where {T <: SymbolicObject} =
    sympy.linsolve(convert(SymMatrix, V), args...; kwargs...)
linsolve(Ts::Tuple, args...; kwargs...) where {T <: SymbolicObject} =
    sympy.linsolve(Ts, args...; kwargs...)


## dsolve
dsolve(eqn::Sym; kwargs...) = sympy.dsolve(eqn; kwargs...)

"""
   dsolve(eqn, var, args..,; kwargs...)

Solve IVP problem.

Example:

```
x = Sym("x")
y = SymFunction("y")
eqn = y''(x) - y(x) - exp(x)
dsolve(eqn, x, (y,0,1), (y, 1, 1//2))
```
"""
function dsolve(eqn::Sym, var::Sym, args::Tuple...; kwargs...)

    if length(args) == 0
        throw(ArgumentError("""Some initial value specification is needed.
Specifying the function, as in `dsolve(ex, f(x))`, is deprecated.
Use `sympy.dsolve(ex, f(x); kwargs...)` directly for that underlying interface.
"""))
    end

    out = sympy.dsolve(eqn; kwargs...)
    ord = ode_order(eqn, var)

    ## `out` may be an array of solutions. If so we do each one.
    ## we want to use an array for output only if needed
    if !isa(out, Array)
        return _solve_ivp(out, var, args,ord)
    else
        output = Sym[]
        for o in out
            a = _solve_ivp(o, var, args,ord)
            a != nothing && push!(output, a)
        end
        return length(output) == 1 ? output[1] : output
    end
end
export dsolve

## Helper.
## out is an equation in var with constants. Args are intial conditions
## Return `nothing` if initial condition is not satisfied (found by `solve`)
function _solve_ivp(out, var, args, o)

    eqns = Sym[(diff(out.rhs(), var, f.n))(var=>x0) - y0 for (f, x0, y0) in args]
    sols = solve(eqns, Sym["C$i" for i in 1:o])
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
