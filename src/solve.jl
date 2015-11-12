"""

Solve an expression for any zeros or a system of expressions passed a vector.

Examples: 

```
x,y, a,b,c = symbols("x, y, a, b, c", real=true)
solve(x^2 - x + 1)
solve(a*x^2  + b*x + c, x)
solve([x - y - 1, x + y - 2], [x,y])
solve([x - y - 1, x + y - 2])   # same as above, as it will assume all free variables.
```

Can solve a single expression for a lone free variable, or if a variable is passed as a second argument, solve for that variable in terms of the others.

When passed a vector of expressions, `solve` looks for solutions to
the system of m equations with n unknowns, where the equations and
unknowns are passed as `Vector{Sym}`. If the unknowns are not
specified, all the free symbols will be used.


The `SymPy` docs say this about `solve`:

> Note If solve returns [] or raises NotImplementedError, it
> doesn’t mean that the equation has no solutions. It just means
> that it couldn’t find any. Often this means that the solutions
> cannot be represented symbolically. For example, the equation
> `x=cos(x)` has a solution, but it cannot be represented
> symbolically using standard functions.
> 
> In fact, solve makes no guarantees whatsoever about the completeness
> of the solutions it finds. Much of solve is heuristics, which may find
> some solutions to an equation or system of equations, but not all of
> them.
        

The return value depends on the inputs:

* If there is one equation with one specified variable (either explicit, or because `free_symbols` returns only one variable), the return value is an array of solutions.

* Otherwise, if there is a unique solution found a dictionary is returned. 

* Otherwise, if there is 0 or more than one solution found, an array of dictionaries is returned.

[Note: this may change. The current arrangement is perhaps more convenient, but having a consistent return type has other advantages.]

Note: The individual components of the array display more nicely than the array.

Reference: [SymPy Docs](http://docs.sympy.org/0.7.5/modules/solvers/solvers.html#algebraic-equations)

"""  
function solve{T<:Sym}(ex::(@compat Union{T,Vector{T}});  kwargs...)
    ## No symbols specified? Find them
    xs = free_symbols(ex)
    if length(xs) ==0 
        error("The expression has non free variables")
    elseif length(xs) == 1
        xs = xs[1]
    end
    solve(ex, xs; kwargs...)
end

## solve for a single variable, Return Sym[]
function solve(ex::Sym, x::Sym; kwargs...)
    a = sympy_meth(:solve, ex, x;  kwargs...)
    
    ## Way too much work here to finesse into a nice enough output
    ## (Issue comes from solving without specifying variable when 2 or more variables in the expression
    isa(a, Dict) && return(a)
    length(a) == 0 && return(a)
    if (length(a) > 0 && isa(a[1], Dict))
        d = Dict()
        for kv in a
            for (k,v) in kv
                d[k] = v
            end
        end
        d
    else
        Sym[v for v in a]
    end
end

function _mapdict(out, xs) ## can be a tuple if m=n
    d = Dict{Sym, Sym}()
    [d[xs[i]] = out[i] for i in 1:length(out)]
    d
end
function _mapdict(out::Dict,xs=nothing)
    d = Dict{Sym,Sym}()
    [d[k]=v for (k,v) in out]
    d
end

## Solve for a single variable from equations. Return Dict{Sym,Sym}
function solve{T<:Sym}(exs::(@compat Union{T,Vector{T}}), x::Sym; kwargs...)
    solve(exs, [x;]; kwargs...)
end

function solve{T<:Sym,S<:Sym}(exs::(@compat Union{T,Vector{T}}), xs::Vector{S}; kwargs...)
    a = sympy_meth(:solve, exs, xs;  kwargs...)
    ## nicer output
    if isa(a, Dict)
        _mapdict(a)              # XXX type unstable! should be array... Should we change? XXX
    else
        [_mapdict(_,xs) for _ in a]
    end
end


## Numeric solutions

"""
Numerically solve for a zero of an expression.

Examples: 
```
solve(x^5 - x -1) # inconclusive
nsolve(x^5 - x - 1, 1)
```

Reference: [SymPy Docs](http://docs.sympy.org/0.7.5/modules/solvers/solvers.html#algebraic-equations)
"""              
nsolve(ex::Sym, x::Sym, x0::Number) = sympy_meth(:nsolve, project(ex), project(x), x0) |> x -> convert(Float64, x)
nsolve(ex::Sym, x0::Number) =  sympy_meth(:nsolve, project(ex), x0) |> x -> convert(Float64, x)
function nsolve{T <: Number}(ex::Vector{Sym}, x::Vector{Sym}, x0::Vector{T}; kwargs...)
    out = sympy_meth(:nsolve, tuple(map(project,ex)...), tuple(map(project,x)...), tuple(x0...); kwargs...)
    ## ans is matrix object -- convert
    out = convert(Array{Sym}, sympy[:Matrix](out))
    map(x -> convert(Float64, x), out)
end
export nsolve

## dsolve
## Make a function argument, but munge arguments from Sym -> PyObject class
SymFunction(nm::SymOrString) = (args...) -> Sym(sympy[:Function](nm)(project(args)...))


"""

Solve an odinary differential equation.

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

