
## Call
## Call symbolic object with natural syntax
## ex(x=>val)
## how to do from any symbolic object?
(ex::Sym)() = ex
function (ex::Sym)(args...)
    xs = ex.free_symbols
    for (var, val) in zip(xs, args)
        ex = ex.subs(var, val)
    end
    ex
end
function (ex::Sym)(x::Dict)
    for (k,v) in x
        ex = ex.subs(k, v)
    end
    ex
end
function (ex::Sym)(x::Pair...)
    for (k,v) in x
        ex = ex.subs(k, v)
    end
    ex
end

## use python iteration

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
## name of symbolic object
function __name__(x)
    try
        x.__class__.__name__
    catch err
        ""
    end
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

## """
##     from_import(PyCallModule; Ms, fns)

## Mimic `from module import *` within `Julia`

## * searches for all functions in PyCallModule, and creates a Julia function specialized on the first argument being a symbolic object.
## * If this function is exported by a module in Ms, it is extended, if not, it is exported.

## This only imports function objects. Classes are not imported. For example, `sympy.Matrix` is needed and not `Matrix`.
## """
## function from_import_all(sm;
##                          Ms=(Base, LinearAlgebra, SpecialFunctions,Base.MathConstants),
##                          fns=nothing,
##                          exclude = ()
##                          )


##     for (k,v) in (fns == nothing ? _get_member_functions(sm, exclude) : fns)
##         meth = Symbol(k)
##         inMs = false
##         for M in Ms
##             if isdefined(M, meth)
##                 inMs = true
## #                @show k
##                 @eval begin
##                     ($M.$meth)(ex::SymbolicObject, args...; kwargs...) =
##                         getproperty($sm,$k)(ex, args...; kwargs...)
##                 end
##                 break
##             end
##         end
##         if !inMs
##             ## need to export
## #            @show "export", k
##             @eval begin
##                 ($meth)(ex::SymbolicObject, args...; kwargs...) =
##                     getproperty($sm,$k)(ex, args...; kwargs...)
##             end
##             eval(Expr(:export, meth))
##         end
##     end
## end

## """
##     from_import(PyCallModule, meths; Ms)

## Mimics `from module import a, b, c`.

## This is similar to `from_import` only just those specified methods in `meths` are considered.
## """
## function from_import(sm, meths;
##                      Ms=(Base, LinearAlgebra, SpecialFunctions, Base.MathConstants))
##     mems = PyCall.inspect.getmembers(sm)
##     fns = Dict()
##     for (m,p) in mems
##         if Symbol(m) in meths
##             fns[m] = p
##         end
##     end
##     from_import_all(sm; Ms=Ms, fns=fns)
## end

base_Ms = (Base, SpecialFunctions, Base.MathConstants,
           LinearAlgebra, OffsetArrays
           )

base_exclude=("C", "lambdify",
              "latex", "eye", "sympify",
              "div",
              "dsolve",
              "ask",
              "plot")

"""
    import_from(module, meths; kwargs...)

Import methods from python module

* `module`: a python module, such as `sympy`
* `meths`: nothing or a tuple of symbols to import. If `nothing`, then all member functions of the module are imported (but not constructors and other objects)
* `Ms`: additional Julia Modules to import from. By default, a few base modules are searched for to avoid namespace collisions.
* `typ`: a symbol indicating variable type first argument of new function should be restricted to. For most, the default, `:SymbolicObject` will be appropriate
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
import PyCall: PyObject, hasproperty
export func, args, funcname


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



end
