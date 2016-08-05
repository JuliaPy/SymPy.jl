## dsolve

## A type akin to SymFunction but with the ability to keep track of derivative
type SymFunction <: SymPy.SymbolicObject
    u::PyCall.PyObject
    n::Int
end
    
"""

Create a symbolic function. These can be used for specifying differential equations.
For these objects we can specify derivatives with the transpose
operator (e.g., `u''`) as opposed to, say `diff(u(x), x, 2)`.

Example:
```
u = SymFunction("u")
u'
```

Alternatively, we can pass `symfunction` to the `cls` argument of
`symbols`. This provides a convenient way to create more than one
symbolic function per call.

```
F, G = symbols("F,G", cls=symfunction)
```

"""
function SymFunction{T<:AbstractString}(x::T)
    u = sympy[:Function](x)
    SymFunction(u, 0)
end

IVPSolution{T<:AbstractString}(x::T) = SymFunction(x)
export IVPSolution
## Need to deprecate this...
@deprecate IVPSolution(x::AbstractString) SymFunction(x)

symfunction(x) = SymFunction(x) # for use with symbols("F", cls=symfunction)

## macro to create functions
"""

Macro to create multiple symbolic functions in the Main workspace.

Example:
```
@symfuns Vd Vq id iq
@syms Rs Lq=>"\lambda_q"  Ld=>"lambda_d" FLq FLd We Wm Np FLf t

basicDQEquations = [
    -Vq(t) + Rs*iq(t) + We*Ld*id(t) + We*FLf + Lq*iq'(t)
    -Vd(t) + Rs*id(t) - We*Lq*iq(t) + Ld*id'(t) ]
```


Thanks to `@alhirzel` for the contribution.
"""
macro symfuns(x...)
    q = Expr(:block)
    fs = []    # running list of functions created
    for s in x
        if isa(s, Expr) && s.head == :(=>) # named function
            push!(fs, s.args[1])
            push!(q.args, Expr(:(=), s.args[1], Expr(:call, :SymFunction, s.args[2])))
        elseif isa(s, Symbol)
            push!(fs, s)
            push!(q.args, Expr(:(=), s, Expr(:call, :SymFunction, string(s))))
        else
            throw(AssertionError("@fns expected a list of symbols"))
        end
    end
    push!(q.args, Expr(:tuple, fs...)) # return all of the functions we created
    eval(Main, q)
end



# some display of objects
Base.display(u::SymFunction) = println("$(string(Sym(u.u)))" * repeat("'", u.n))

## override call is in call.jl


## rather than use `diff(u(x),x,1)` we can use `u'(x)`
function Base.ctranspose(x::SymFunction)
    SymFunction(x.u, x.n + 1)
end


"""

Solve an ordinary differential equation.

Examples:

```
f = SymFunction("f")
x = Sym("x")
dsolve(diff(f(x), x) + f(x)) ## solve f'(x) + f(x) = 0
dsolve(diff(f(x), x, x) + f(x)) ## solve f''(x) + f(x) = 0
```

References: [SymPy Docs](http://docs.sympy.org/0.7.5/modules/solvers/ode.html#ode-docs)

In addition, we add a `Julia`n interface to solve initial-value
problems. (Julia `v"0.4.0"+`) For this, the calling pattern is

`dsolve(eqn::Sym, var::Sym, args::Tuple...; kwargs...)` where

* `eqn` the equation. Can be written as `u'(x) - u(x)` where `u` is of type `SymFunction`
* `var` the variable of the equation. Typically `free_symbols(x)[1]`, but still, for now, is specified
* `args::Tuple...` Specification of initial values in style of `(u, x0, y0)` or `(u', x0, y0)`.
* `kwargs...` are passed onto `dsolve`

Example:

```
u = SymFunction("u")
@vars x
dsolve(u'(x) - 2u(x))                  # u(x) = C_1 e^(2x)
dsolve(u'(x) - 2u(x), x, (u, 0 , 1)) # u(x) = e^(2x)
dsolve(u''(x) - 2u(x), x, (u, 0, 1), (u', 0, 2))  ## some expression
```

This is a simple wrapper for: 1) substituting in the values for the
initial conditions 2) calling `solve` for the constants `C1`, `C2`,
... 3) putting these values back into the equations. If this automated
approach breaks down, then those steps can be done manually starting
with the output of `dsolve` without the initial conditions.

"""             

dsolve(ex::Sym;kwargs...) = sympy_meth(:dsolve, ex; kwargs...)
dsolve(exs::Vector{Sym};kwargs...) = sympy_meth(:dsolve, exs; kwargs...)
dsolve(exs::Vector{Sym}, fx::Sym; kwargs...) = sympy_meth(:dsolve, exs, fx; kwargs...)
## Note, dsolve(ex, var::Sym; kwargs...) is depracated so that initial value problems can be specifiedx


## The dsolve function is not great for initial value problems
## The `ics` argument seems to only work with power series solutions
##
## This adds the ability to more naturally specify the equations.
##
## There are issues with the number of solutions that need ironing out.
function dsolve(eqn::Sym, var::Sym, args::Tuple...; kwargs...)

    if length(args) == 0
        throw(ArgumentError("""Some initial value specification is needed.
Specifying the function, as in `dsolve(ex, f(x))`, is deprecated.
Use `sympy_meth(:dsolve, ex, f(x); kwargs...)` directly for that underlying interface.
"""))
    end
    
    out = dsolve(eqn; kwargs...)
    ord = sympy_meth(:ode_order, eqn, var)

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

## Helper.
## out is an equation in var with constants. Args are intial conditions
## Return `nothing` if initial condition is not satisfied (found by `solve`)
function _solve_ivp(out, var, args, o)
    eqns = Sym[rhs(diff(out, var, f.n))(var=>x0) - y0 for (f, x0, y0) in args]
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

export SymFunction, symfunction, dsolve
