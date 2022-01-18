##################################################

"""
    SymFunction

A type and constructor to Create symbolic functions. Such objects can be used
for specifying differential equations. The macro [`@syms`](@ref) is also available for constructing `SymFunction`s (`@syms f()`)

## Examples:

```jldoctest symfunction
julia> using SymPy

julia> u = SymFunction("u");

julia> @syms v();

```

Alternatively, we can pass a comma separated string of variable names to create
more than one at a time.

```jldoctest symfunction
julia> F,G,H = SymFunction("F, G, H")
3-element Vector{SymFunction}:
 F
 G
 H
```


For symbolic functions *not* wrapped in the `SymFunction` type, the `sympy.Function` constructor can be used, as can the [`symbols`](@ref) function to construct symbolic functions (`F=sympy.Function(F,"real=true")`; `F = sympy.symbols("F", cls=sympy.Function, real=true)`).

```jldoctest symfunction
julia> @syms u(), v()::real, t
(u, v, t)

julia> sqrt(u(t)^2), sqrt(v(t)^2) # real values have different simplification rules
(sqrt(u(t)^2), Abs(v(t)))

```

Such functions are undefined functions in SymPy, and can be used symbolically, such as with taking derivatives:

```
@syms x y u()
diff(u(x), x)
diff(u(x, y), x)
```


Here is one way to find the second derivative of an inverse function to `f`, utilizing the `SymFunction` class and the convenience `Differential` function:

```
@syms f() f⁻¹() x
D = Differential(x) # ∂(f) is diff(f(x),x)
D² = D∘D
u1 = solve(diff((f⁻¹∘f)(x), x) ~ 1, D(f⁻¹)(f(x)))[1]
u2 = solve(diff((f⁻¹∘f)(x), x,2) ~ 0, D²(f⁻¹)(f(x)))[1]
u2(D(f⁻¹)(f(x)) => u1) # f''/[f']^3
```

"""
SymFunction

## SymFunction has a `n` field to allow u', u'', etc to work
## However, it seems better to be more consistent ModelingToolkit
## and use D = Differential(x) to inicate derivatives, e.g. D(u)
## This `n` will be deprecated
mutable struct SymFunction <: SymbolicObject
    __pyobject__::PyCall.PyObject
    n::Int
end


function SymFunction(x::T; kwargs...) where {T<:AbstractString}
    us = split(x, r",\s*")
    length(us) > 1 ? SymFunction.(us; kwargs...) :
        SymFunction(sympy.Function(x; kwargs...), 0)
    # SymFunction(sympy.Function(x; kwargs...))  # if n removed
end


# Steal this idea from ModelingToolkit
# better than the **hacky** f'(0) stuff
"""
    Differential(x)

Use to find (partial) derivatives.

## Example
```
@syms x y u()
Dx = Differential(x)
Dx(u(x,y))  # resolves to diff(u(x,y),x)
Dx(u)       # will evaluate diff(u(x), x)
```
"""
struct Differential
    x::Sym
end
(∂::Differential)(u::Sym) = diff(u, ∂.x)
(∂::Differential)(u::SymFunction) = diff(u(∂.x), ∂.x)
export Differential

# all that's needed but using things to deprecate now
# Base.show(io::IO, u::SymFunction) = print(io, "$(string(Sym(u.__pyobject__)))")
#(u::SymFunction)(args...) = u.__pyobject__(PyObject.(args)...)

## -----------------------

## ---- deprecate. Remove once u'(x) syntax is removed ----
## support legacy

# Hacky way to streamline expressions like
# dsolve(diff(u(x),x) ~ u(x), ics=Dict(diff(u(x),x)(0) => 1))
function Base.adjoint(x::SymFunction)
    Base.depwarn("""Use of `'` to indicate a derivative of a symbolic function will be deprecated. Use `diff(u(x),x)` or `D=Differential(x); D(u)`""", :adjoint)
    SymFunction(x.__pyobject__, x.n + 1)
end


Base.:(==)(x::SymFunction, y::SymFunction) = x.__pyobject__ == y.__pyobject__ && x.n == y.n
Base.hash(x::SymFunction) = hash((hash(x.__pyobject__),x.n))

# these are from https://github.com/OptMist-Tokyo/DAEPreprocessor.jl/blob/sympy_warning/src/symbolic.jl
derivative(x::SymFunction, d::Int = 1) = SymFunction(x.__pyobject__, x.n + d)

Base.show(io::IO, u::SymFunction) = print(io, "$(string(Sym(u.__pyobject__)))" * repeat("'", u.n))
Base.show(io::IO, ::MIME"text/plain", u::SymFunction) = print(io, "$(string(Sym(u.__pyobject__)))" * repeat("'", u.n))
Base.show(io::IO, ::MIME"text/latex", x::SymFunction) = print(io, as_markdown("\\begin{align*}" * latex(x) * "\\end{align*}"))
function Base.show(io::IO, ::MIME"text/latex", x::AbstractArray{SymFunction, 1})
    print(io, as_markdown("\\begin{align*}\\left[\\begin{array}{c}" * join(latex.(x), "\\\\") * "\\end{array}\\right]\\end{align*}"))
end

latex(x::SymFunction) = latex(Sym(x.__pyobject__)) * repeat("'", x.n)

function PyCall.PyObject(f::SymFunction)
    f.n == 0 && return f.__pyobject__
    z = symbols(gensym())
    f(z).__pyobject__  #  diff(f(__x__), __x__, f.n).__pyobject__
end


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


## ---- deprecate macro; use @syms ----

"""
    @symfuns

Thanks to `@alhirzel` for the contribution.

!!! Note:
    The `@symfuns` macro will be deprecated. The more general [`@syms`](@ref) macro should be used for constructing symbolic functions of type `SymFunction` and `symbols` can be used to construct symbolic functions in general.

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
