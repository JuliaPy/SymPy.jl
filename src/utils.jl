
## Call
## Call symbolic object with natural syntax
## ex(x=>val)
## how to do from any symbolic object?
(ex::Sym)() = ex

## without specification, variables to substitute for come from ordering of `free_symbols`:
function (ex::Sym)(args...)
    xs = ex.free_symbols
    for (var, val) in zip(xs, args)
        ex = ex.subs(var, val)
    end
    ex
end

## can use a Dict or pairs to specify:
function (ex::Sym)(x::Dict)
    for (k,v) in x
        ex = ex.subs(k, v)
    end
    ex
end
function (ex::Sym)(kvs::Pair...)
    for (k,v) in kvs
        ex = ex.subs(k, v)
    end
    ex
end

##################################################
## subs
##
"""
`subs` is used to subsitute a value in an expression with another
value.
Examples:

```
x,y = symbols("x,y")
ex = (x-y)*(x+2y)
subs(ex, (y, y^2))
subs(ex, (x,1), (y,2))
subs(ex, (x,y^3), (y,2))
subs(ex, y, 3)
```

There is a curried form of `subs` to use with the chaining `|>` operator

```
ex |> subs(x,e)
```
The use of pairs gives a convenient alternative:

```
subs(ex, x=>1, y=>2)
ex |> subs(x=>1, y=>2)
```

Examples:

```
subs(ex, :y, pi)    # using a symbol, not a symbolic object
subs(ex, x=1, y=pi) # using keyword argument, and not pairs
## or their curried or call forms
ex |> subs(:x, e)
ex |> subs(x=e)
ex(x=2, y=3)
```

"""
subs(ex::T, y::Tuple{Any, Any}; kwargs...)          where {T <: SymbolicObject} = ex.subs(y[1], Sym(y[2]), kwargs...)
subs(ex::T, y::Tuple{Any, Any}, args...; kwargs...) where {T <: SymbolicObject} = subs(subs(ex, y), args...)
subs(ex::T, y::S, val; kwargs...)                   where {T <: SymbolicObject, S<:SymbolicObject} = subs(ex, (y,val))
subs(ex::T, dict::Dict; kwargs...)                  where {T <: SymbolicObject} = subs(ex, dict...)
subs(ex::T, d::Pair...; kwargs...)                  where {T <: SymbolicObject} = subs(ex, ((p.first, p.second) for p in d)...)
subs(exs::Tuple{T, N}, args...; kwargs...)          where {T <: SymbolicObject, N} = map(u -> subs(u, args...;kwargs...), exs)
subs(x::Number, args...; kwargs...) = x

## curried versions to use with |>
subs(x::SymbolicObject, y; kwargs...) = ex -> subs(ex, x, y; kwargs...)
subs(;kwargs...)                      = ex -> subs(ex; kwargs...)
subs(dict::Dict; kwargs...)           = ex -> subs(ex, dict...; kwargs...)
subs(d::Pair...; kwargs...)           = ex -> subs(ex, [(p.first, p.second) for p in d]...; kwargs...)


##################################################
# avoid type piracy. After we call `pytype` mappings, some
# objects are automatically converted and no longer PyObjects
pycall_hasproperty(x::PyCall.PyObject, k) = PyCall.hasproperty(x, k)
pycall_hasproperty(x::Sym, k) = PyCall.hasproperty(PyCall.PyObject(x), k)
pycall_hasproperty(x, k) = false

# simple helper for boolean properties
# x.is_finite -> is_(:finite, x)
# e.g.: is_(:FiniteSet, x) = hasproperty && get property
function is_(k::Symbol, x::Sym)::Bool
    key = Symbol("is_$k")
    pycall_hasproperty(x, key) && getproperty(x, key) == Sym(true)
end


_get_members(sm) = PyCall.inspect.getmembers(sm)
function _get_member_functions(sm, exclude=())
    mems = _get_members(sm)
    fns = Dict()
    for (u, v) in mems
        # special cases
        u in exclude && continue

        pycall_hasproperty(v, :deprecated) && continue
        !isa(v, PyCall.PyObject) && continue

        ## we grab functions or FunctionClass objects only
        if PyCall.hasproperty(v, :__class__) &&
            v.__class__.__name__ in ("function", "FunctionClass")
            fns[u] = v
        end
    end
    fns
end

function _is_module(x::Symbol)
    try
        typeof(eval(x)) <:  Module && x ≠ :Main
    catch err
        false
    end
end

function loaded_modules()
    nms = names(Main,imported=true)
    nms = filter(nm -> nm != :SymPy, nms)
    Ms = filter(_is_module,  nms)
    eval.(Ms)
end

# default list of modules to search for namespace collicsions
const base_Ms = (Base, SpecialFunctions, Base.MathConstants,
           LinearAlgebra, OffsetArrays
           )

# default list of methods to exclude from importing
#
# In addition to issues (such as "C") this should list
#
# * julia methods for which the sympy method is different from julia's generic usage (e.g. `div`)
# * `julia` methods that are clearly issues with the package system, though not in base_Ms (e.g., `plot`, `latex`)
# * sympy methods for which a substitute is used (e.g. `lambdify`)
#
# these are still accesible through dot-call syntax.
#
const base_exclude=("C", "lambdify",
              "latex", "eye", "sympify","symbols", "subs",
              "div", "log", "sinc",
              "dsolve",
              "ask",
              "plot")

"""
    import_from(module, meths; kwargs...)

Import methods from python module

* `module`: a python module, such as `sympy`
* `meths`: nothing or a tuple of symbols to import. If `nothing`, then all member functions of the module are imported (but not constructors or other objects)
* `Ms`: additional Julia Modules to import from. By default, a few base modules are searched for to avoid namespace collisions.
* `typ`: a symbol indicating variable type for first argument that the new function should be restricted to. For most, the default, `:SymbolicObject` will be appropriate
* `exclude`: when importing all (`meths=nothing`), this can be used to avoid importing some methods by name. The default has a few to avoid.

Examples:

```
import_from(sympy)  # bring in functions from sympy (done `import_sympy`)
import_from(sympy, (:sin, :cos))  # just bring in a few methods
import_from(sympy , (:Wild,), typ=:Any) # Allows `Wild("x")`
#
import PyCall
PyCall.pyimport_conda("sympy.physics.wigner", "sympy")
import_from(sympy.physics.wigner)
```

"""
function import_from(sm, meths=nothing;
                     Ms::Tuple=(),
                     typ::Symbol=:SymbolicObject,
                     exclude::Union{Nothing, NTuple{N,Symbol}}=nothing
                     ) where {N}


    if meths == nothing
        _exclude = isa(exclude, Nothing) ? base_exclude : exclude
        fns = _get_member_functions(sm, _exclude)
    else
        mems = PyCall.inspect.getmembers(sm)
        fns = Dict()
        for (m,p) in mems
            if Symbol(m) in meths
                fns[m] = p
            end
        end
    end

     for (k,v) in fns
         meth = Symbol(k)
         inMs = false
         for M in union(base_Ms, Ms)
             if isdefined(M, meth)
                inMs = true
#                @show k
                 @eval begin
                     ($M.$meth)(ex::($typ), args...; kwargs...) =
                         getproperty($sm,$k)(ex, args...; kwargs...)
                 end
                 break
            end
        end
        if !inMs
            ## need to export
#            @show "export", k
            @eval begin
                ($meth)(ex::($typ), args...; kwargs...) =
                    getproperty($sm,$k)(ex, args...; kwargs...)
            end
            eval(Expr(:export, meth))
        end
    end

end



##
function free_symbols(ex::Union{T, Vector{T}}) where {T<:SymbolicObject}
    pex = PyObject(ex)
    #fs.__class__.__name__ == "set"
    if PyCall.hasproperty(pex, :free_symbols)
        convert(Vector{Sym}, collect(pex.free_symbols))
    else
        Sym[]
    end
end



## Module for Introspection
module Introspection

import SymPy: Sym
import PyCall: PyObject, hasproperty, PyNULL, inspect
export args, func, funcname, class, classname, getmembers


# utilities

"""
    funcname(x)

Return name or ""
"""
function funcname(x::Sym)
    y = PyObject(x)
    if hasproperty(y, :func)
        return y.func.__name__
    else
        return ""
    end
end

"""
   func(x)

Return function head from an expression

[Invariant:](http://docs.sympy.org/dev/tutorial/manipulation.html)

Every well-formed SymPy expression `ex` must either have `length(args(ex)) == 0` or
`func(ex)(args(ex)...) = ex`.
"""
func(ex::Sym) = return ex.func

"""
    args(x)

Return arguments of `x`, as a tuple. (Empty if no `:args` property.)
"""
function args(x::Sym)
    if hasproperty(PyObject(x), :args)
        return x.args
    else
        return ()
    end
end

function class(x::T) where {T <: Union{Sym, PyObject}}
    if hasproperty(PyObject(x), :__class__)
        return x.__class__
    else
        return PyNull()
    end
end

function classname(x::T) where {T <: Union{Sym, PyObject}}
    cls = class(x)
    if cls == PyNULL()
        "NULL"
    else
        cls.__name__
    end
end

function getmembers(x::T) where {T <: Union{Sym, PyObject}}
    Dict(u=>v for (u,v) in inspect.getmembers(x))
end

end
