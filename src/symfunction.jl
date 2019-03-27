##################################################

"""
     SymFunction

Thin wrapper for symbolic functions that allows prime notation in place of using `diff`.

Functions constructed through `SymFunction("f")` or `@symfuns f`.
"""
mutable struct SymFunction <: SymbolicObject
    x::PyCall.PyObject
    n::Int
end

Base.show(io::IO, u::SymFunction) = print(io, "$(string(Sym(u.x)))" * repeat("'", u.n))

"""


Create a symbolic function. These can be used for specifying differential equations.
For these objects we can specify derivatives with the transpose
operator (e.g., `u''`) as opposed to, say `diff(u(x), x, 2)`.

Example:
```
u = SymFunction("u")
u'
```

Alternatively, we can pass a comma separated string of variable names to create
more than one at a time. (The `cls=symfunction` is no longer supported):

```
F,G,H = SymFunction("F, G, H")
```

"""
function SymFunction(x::T) where {T<:AbstractString}
    us = split(x, r",\s*")
    if length(us) > 1
        map(u -> SymFunction(sympy."Function"(u), 0), us)
    else
        SymFunction(sympy."Function"(x), 0)
    end
#    u = sympy."Function"(x)
#    SymFunction(u, 0)
end

"""
    symfuns...
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
    Core.eval(Main, q)
end



function PyCall.PyObject(f::SymFunction)
    f.n == 0 && return f.x
    z = symbols(gensym())
    f(z).x  #  diff(f(__x__), __x__, f.n).x
end

Base.adjoint(x::SymFunction) = SymFunction(x.x, x.n + 1)

(u::SymFunction)(x::Base.Dict) = throw(ArgumentError("IVPsolutions can only be called with symbolic objects"))
(u::SymFunction)(x::Base.Pair) = throw(ArgumentError("IVPsolutions can only be called with symbolic objects"))
function (u::SymFunction)(x)
    if u.n == 0
        PyObject(u)(x) #u.x(PyObject(x))
    else
        __x = Sym("__x")
        diff(u.x(PyObject(__x)), __x, u.n)(__x => x)
    end
end

(u::SymFunction)(x, y...) = u.n== 0 ? u.x(map(PyObject, vcat(x, y...))...) : error("Need to implement derivatives of symbolic functions of two or more variables")
