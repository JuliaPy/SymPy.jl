## dsolve

## Make a function argument, but munge arguments from Sym -> PyObject class
if VERSION < v"0.4.0"
    SymFunction(nm::SymOrString) = (args...) -> Sym(sympy[:Function](nm)(project(args)...))
else


    ## A type akin to SymFunction but with the ability to keep track of derivative
    type SymFunction <: SymPy.SymbolicObject
        u::PyCall.PyObject
        n::Int
    end
    
    """

Create a symbolic solution to an initial value problem.  This is like
`SymFunction`, only we can specify derivatives with the transpose
operator (e.g., `u''`) as oppd to, say `diff(u(x), x, 2)`.

Example:
```
u = SymFunction("u")
u'
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
    
    # some display of objects
    Base.display(u::SymFunction) = println("$(string(Sym(u.u)))" * repeat("'", u.n))
    
    ## override call
    Base.call(u::SymFunction, x::Base.Dict) = throw(ArgumentError("IVPsolutions can only be called with symbolic objects"))
    Base.call(u::SymFunction, x::Base.Pair) = throw(ArgumentError("IVPsolutions can only be called with symbolic objects"))
    function Base.call(u::SymFunction, x) 
        if u.n == 0
            u.u(SymPy.project(x))
        else
            __x = Sym("__x")
            diff(u.u(__x.x), __x, u.n)(x)
        end
    end
    


    ## rather than use `diff(u(x),x,1)` we can use `u'(x)`
    function Base.ctranspose(x::SymFunction)
        SymFunction(x.u, x.n + 1)
    end
    
    
end


"""

Solve an ordinary differential equation.

Examples:

```
f = SymFunction("f")
x = Sym("x")
dsolve(diff(f(x), x) + f(x), f(x)) ## solve f'(x) + f(x) = 0
dsolve(diff(f(x), x, x) + f(x), f(x)) ## solve f''(x) + f(x) = 0
```

References: [SymPy Docs](http://docs.sympy.org/0.7.5/modules/solvers/ode.html#ode-docs)
"""             
dsolve(ex::Sym, fx::Sym;kwargs...) = sympy_meth(:dsolve, ex, fx; kwargs...)
dsolve(ex::Sym;kwargs...) = sympy_meth(:dsolve, ex; kwargs...)
dsolve(exs::Vector{Sym};kwargs...) = sympy_meth(:dsolve, exs; kwargs...)
dsolve(exs::Vector{Sym}, fx::Sym; kwargs...) = sympy_meth(:dsolve, exs, fx; kwargs...)



## The dsolve function is not good with initial values problems
## The `ics` argument seems to only work with power series solutions
##
## This adds the ability to more naturally specify the equations.
##
## This interface is subject to change"""

"""
Solve an intial value problem

* `eqn` the equation. Can be written as `u'(x) - u(x)` where `u` is of type `SymFunction`
* `var` the variable of the equation. Typically `free_symbols(x)[1]`, but still, for now, is specified
* `args::Tuple...` Specification of initial values in style of `(u, x0, y0)` or `(u', x0, y0)`.
* `kwargs...` are passed onto `dsolve`

Example:

```
u = SymFunction("u")
@vars x
dsolve(u'(x) - 2u(x))                  # u(x) = C_1 e^(2x)
ivpsolve(u'(x) - 2u(x), x, (u, 0 , 1)) # u(x) = e^(2x)
ivpsolve(u''(x) - 2u(x), x, (u, 0, 1), (u', 0, 2))  ## some expression
```

"""
function ivpsolve(eqn, var::Sym, args::Tuple...; kwargs...)
    out = dsolve(eqn; kwargs...)
    
    eqns = Sym[rhs(diff(out, var, f.n))(var=>x0) - y0 for (f, x0, y0) in args]
    o = sympy_meth(:ode_order, eqn, var)
    sols = solve(eqns, Sym["C$i" for i in 1:o])

    ## massage output
    ## Might have more than one solution, though unlikely. But if we substitute a variable
    ## for y0 we will get an array back from solve which may have length 1.
    if isa(sols, Array)
        if length(sols) == 1
            sols = sols[1]
        else
            return [out([k=>v for (k,v) in sol]...) for sol in sols]
        end
    end

    out([k=>v for (k,v) in sols]...)
end

export SymFunction, ivpsolve
