##################################################

"""
     SymFunction

Thin wrapper for symbolic functions that allows prime notation in place of using `diff`.

Functions constructed through `SymFunction("f")` or `@symfuns f`.
"""
mutable struct SymFunction <: SymbolicObject
    __pyobject__::PyCall.PyObject
    n::Int
end

Base.:(==)(x::SymFunction, y::SymFunction) = x.__pyobject__ == y.__pyobject__ && x.n == y.n
Base.hash(x::SymFunction) = hash((hash(x.__pyobject__),x.n))

# these are from https://github.com/OptMist-Tokyo/DAEPreprocessor.jl/blob/sympy_warning/src/symbolic.jl
derivative(x::SymFunction, d::Int = 1) = SymFunction(x.__pyobject__, x.n + d)

Base.show(io::IO, u::SymFunction) = print(io, "$(string(Sym(u.__pyobject__)))" * repeat("'", u.n))
Base.show(io::IO, ::MIME"text/plain", u::SymFunction) = print(io, "$(string(Sym(u.__pyobject__)))" * repeat("'", u.n))
Base.show(io::IO, ::MIME"text/latex", x::SymFunction) = print(io, "\\begin{align*}" * latex(x) * "\\end{align*}")
function Base.show(io::IO, ::MIME"text/latex", x::AbstractArray{SymFunction, 1})
    print(io, "\\begin{align*}\\left[\\begin{array}{c}" * join(latex.(x), "\\\\") * "\\end{array}\\right]\\end{align*}")
end

latex(x::SymFunction) = latex(Sym(x.__pyobject__)) * repeat("'", x.n)

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
function SymFunction(x::T; kwargs...) where {T<:AbstractString}
    us = split(x, r",\s*")
    if length(us) > 1
        map(u -> SymFunction(sympy."Function"(u; kwargs...), 0), us)
    else
        SymFunction(sympy."Function"(x; kwargs...), 0)
    end
end

"""
    symfuns...
Thanks to `@alhirzel` for the contribution.
"""
macro symfuns(x...)
    q = Expr(:block)
    as = []    # running list of assumptions to be applied
    fs = []    # running list of symbols created
    for s in reverse(x)
        if isa(s, Expr)    # either an assumption or a named variable
            if s.head == :(=)
                s.head = :kw
                push!(as, s)
            elseif s.head == :(=>)
                push!(fs, s.args[1])
                push!(q.args, Expr(:(=), s.args[1], Expr(:call, :SymFunction, s.args[2], map(esc,as)...)))
            end
        elseif isa(s, Symbol)   # raw symbol to be created
            push!(fs, s)
            # @show s
            push!(q.args, Expr(:(=), esc(s), Expr(:call, :SymFunction, string(s), map(esc,as)...)))
        else
            throw(AssertionError("@symfuns expected a list of symbols and assumptions"))
        end
    end
    push!(q.args, Expr(:tuple, map(esc,reverse(fs))...)) # return all of the symbols we created
    q
end



function PyCall.PyObject(f::SymFunction)
    f.n == 0 && return f.__pyobject__
    z = symbols(gensym())
    f(z).__pyobject__  #  diff(f(__x__), __x__, f.n).__pyobject__
end

Base.adjoint(x::SymFunction) = SymFunction(x.__pyobject__, x.n + 1)

(u::SymFunction)(x::Base.Dict) = throw(ArgumentError("IVPsolutions can only be called with symbolic objects"))
(u::SymFunction)(x::Base.Pair) = throw(ArgumentError("IVPsolutions can only be called with symbolic objects"))
function (u::SymFunction)(x)
    if u.n == 0
        PyObject(u)(x) #u.__pyobject__(PyObject(x))
    else
        __x = Sym("__x")
        diff(u.__pyobject__(PyObject(__x)), __x, u.n)(__x => x)
    end
end

(u::SymFunction)(x, y...) = u.n== 0 ? u.__pyobject__(map(PyObject, vcat(x, y...))...) : error("Need to implement derivatives of symbolic functions of two or more variables")
