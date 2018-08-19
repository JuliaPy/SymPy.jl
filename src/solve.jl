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
function solve(ex::Union{T,Vector{T}};  kwargs...) where {T<:Sym}
    ## No symbols specified? Find them
    xs = free_symbols(ex)
    if length(xs) ==0
        error("The expression has no free variables")
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
function solve(exs::Union{T,Vector{T}}, x::Sym; kwargs...) where {T<:Sym}
    
    solve(exs, [x;]; kwargs...)
end

function solve(exs::Union{T,Vector{T}}, xs::Vector{S}; kwargs...) where {T<:Sym,S<:Sym}
    a = sympy_meth(:solve, exs, xs;  kwargs...)
    ## nicer output
    if isa(a, Dict)
        _mapdict(a)              # XXX type unstable! should be array... Should we change? XXX
    else
        [_mapdict(_underscore,xs) for _underscore in a]
    end
end


## Numeric solutions

"""
Numerically solve for a zero of an expression.

Examples:
```
solve(x^5 - x -1) # inconclusive, though `N.(solve(x^5 - x - 1))` works
nsolve(x^5 - x - 1, 1)
```

Returns symbolic values. Use `N`, or some other means, to convert to floating point.

Reference: [SymPy Docs](http://docs.sympy.org/0.7.5/modules/solvers/solvers.html#algebraic-equations)
"""
nsolve(ex::Sym, x::Sym, x0::Number, args...; kwargs...) = sympy_meth(:nsolve, ex, x, x0, args...; kwargs...)
nsolve(ex::Sym, x0::Number,  args...; kwargs...) =  sympy_meth(:nsolve, ex, x0,  args...; kwargs...) 
function nsolve(ex::Vector{Sym}, x::Vector{Sym}, x0::Vector{T}; kwargs...) where {T <: Number}
    out = sympy_meth(:nsolve, tuple(ex...), tuple(x...), tuple(x0...); kwargs...)
    ## ans is matrix object -- convert
    convert(Array{Sym}, sympy["Matrix"](out))
end
export nsolve



"""
`solveset(ex, sym, ...)`     

    SymPy is moving to solveset to replace solve (with 1.0)
    http://docs.sympy.org/latest/modules/solvers/solveset.html

The `solveset` function has the advantage of a consistent return type, a set.

The main interface is `solveset(eqn(s), variable(s); domain=...)`
    
Example
```
out = solveset(x^5 - x - 1)    # set of `CRootof`
as = elements(out)             # An array of same
N.(as)                         # numeric estimates, as would be `N.(solve(x^5 - x - 1))`
solveset(x^2 + 1, x)           # +/- i
solveset(x^2 + 1, x, domain=S.Reals)  # empty set
```
"""    
function solveset(ex::Union{T,Vector{T}};  kwargs...) where {T<:Sym}
    ## No symbols specified? Find them
    xs = free_symbols(ex)
    if length(xs) ==0
        error("The expression has non free variables")
    elseif length(xs) == 1
        xs = xs[1]
    end
    solveset(ex, xs; kwargs...)
end

solveset_sympy_methods = (:solveset,
                          :solveset_real, :solveset_complex,
                          :domain_check)


## solveset does not currently allow simulataneous equations
## Solve for a single variable from equations. Return Dict{Sym,Sym}
# function solveset{T<:Sym}(exs::(Union{T,Vector{T}}), x::Sym; kwargs...)
#     solveset(exs, [x;]; kwargs...)
# end

# function solveset{T<:Sym,S<:Sym}(exs::(Union{T,Vector{T}}), xs::Vector{S}; kwargs...)
#     a = sympy_meth(:solveset, convert(SymMatrix, exs), xs;  kwargs...)
#     ## finesse output
#     a
# end


"""

`linsolve`: solve linear system of equations, return a set

Linsolve can be used to solve:

* An augmented matrix. Input the matrix:

Example:
```
@vars x y a b
M = [2a 1 3; 3b 2 1]   # 2ax + y = 3; 3bx+2y=1
linsolve(M, x, y)
```

The matrix must be of `Matrix{Sym}`, so if used on a numeric problem, the matrix must be coerced, as in
`Sym[1 2 3; 3 2 1]`.
    
* A system of equations. The system is specified as a tuple.

Example
```
linsolve((2a*x+y-3, 3b*x+2y-1), x, y)
linsolve([2a*x+y-3, 3b*x+2y-1], x, y)
```

To get arrays from the sets, the `elements` function is useful:

```
eqs = (2a*x+y-3, 3b*x+2y-1)    
as = linsolve(eqs, x, y) # a FiniteSet
bs = elements(as)        # Array{Any}, but really Array of FiniteSets
cs = elements(bs[1])     # Array of symbolic objects
map(simplify, subs(eqs, x => cs[1], y => cs[2]))  # [0,0]. If 

    
* A system `Ax=b`. A tuple is used to pass in `M` and `b`.

Example
```
M = [2a 1; 3b 2]
B = Sym[3;1]
linsolve((M, B), x, y)
```

[cf.](http://docs.sympy.org/dev/modules/solvers/solveset.html#sympy.solvers.solveset.linsolve)
"""
linsolve(exs::Matrix{T}, args...; kwargs...) where {T<:Sym} = sympy_meth(:linsolve, exs, args...; kwargs...)
linsolve(exs::NTuple{N,T}, args...; kwargs...) where {T<:Sym, N} = sympy_meth(:linsolve, exs, args...; kwargs...)
linsolve(exs::Vector{T}, args...; kwargs...) where {T<:Sym} = sympy_meth(:linsolve, tuple(exs...), args...; kwargs...)
linsolve(exs::Tuple{Array{T,M}, N}, args...; kwargs...) where {T<:Sym, M, N} = sympy_meth(:linsolve, exs, args...; kwargs...)

## may not be there
"""
`nonlinsolve`: solve non-linear system of equations, return a set (may not be defined)

[cf.](http://docs.sympy.org/dev/modules/solvers/solvers.html)
"""
nonlinsolve(exs::Vector{T}, args...; kwargs...) where {T<:Sym} = sympy_meth(:nonlinsolve, exs, args...; kwargs...)

export linsolve, nonlinsolve
