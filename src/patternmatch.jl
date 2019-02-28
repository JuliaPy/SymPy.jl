## http://docs.sympy.org/dev/modules/core.html

"""
     `func(ex)`: Return function head from an expression

```
func(sin(x))  # sin
func(x*y)     # Mul
func(x+y)     # Add
```
"""
func(ex::Sym) = Sym(PyObject(ex).func)
export func


"""

Returns a tuple of arguments

cf. [args](http://docs.sympy.org/latest/modules/core.html#sympy.core.basic.Basic.args)

(args is a property in SymPy, a function call in SymPy.jl.)

Examples
```
Eq(x, x^2) |> args ## (x, x^2)
sin(x) |> args ## (x,)
```

[Invariant:](http://docs.sympy.org/dev/tutorial/manipulation.html)

Every well-formed SymPy expression `ex` must either have `length(args(ex)) == 0` or
`func(ex)(args(ex)...) = ex`.
"""
args(ex::Sym) = ex.args


"""
    `Wild(:x)`: create a "wild card" for pattern matching
"""
Wild(x::AbstractString) = pycall(sympy.Wild, PyAny, x)
Wild(x::Symbol) = Wild(string(x))
export Wild

"""
    `match(pattern, expression, ...)` match pattern against expression

returns a dictionary of matches.

If a match is unsuccesful, returns an *empty* dictionary. (SymPy returns "nothing")

The order of the arguments follows `Julia`'s `match` function, not `SymPy`'s. This may change.
"""
function Base.match(pat::Sym, ex::Sym, args...; kwargs...)
    out = ex.match(pat, args...; kwargs...)
    out == nothing && return Dict()
    out
end

"""
    `replace(expression, pattern, value, ...)`

From: [SymPy Docs](http://docs.sympy.org/dev/modules/core.html)

Traverses an expression tree and performs replacement of matching
subexpressions from the bottom to the top of the tree. The default
approach is to do the replacement in a simultaneous fashion so changes
made are targeted only once. If this is not desired or causes
problems, `simultaneous` can be set to `false`. In addition, if an
expression containing more than one `Wild` symbol is being used to match
subexpressions and the `exact` flag is `true`, then the match will only
succeed if non-zero values are received for each `Wild` that appears in
the match pattern.


Differences from SymPy:

* "types" are specified via calling `func` on the head of an expression: `func(sin(x))` -> `sin`
* functions are supported, but only partially so


Examples (from the SymPy docs)
```
x, y, z = symbols("x, y, z")
f = log(sin(x)) + tan(sin(x^2))
```

Types are specified through `func`:
```
replace(f, func(sin(x)), func(cos(x))) # log(cos(x)) + tan(cos(x^2));   type -> type
```

Wild terms are supported
```
a = Wild("a")
ex = sin(x)
replace(ex, sin(a), cos(a^2))  # cos(x^2)
```

Some patterns allow for functions, but there are subtleties.

The replacement pattern [type -> function](http://docs.sympy.org/dev/modules/core.html#sympy.core.basic.Basic.replace) is case 1.2 and **is not working** (no longer works) as expected:

```
ex = log(sin(x)) + tan(sin(x^2))
replace(ex, func(sin(x)), a -> sin(2a))  # log(sin(2x)) + tan(sin(2*x^2)) # fails now
replace(x*y, func(x*y), (args...) -> sin(2*prod(args)))  # sin(2xy) # fails now
```

The pattern "pattern -> pattern" style works as  expected

```
a = Wild("a")
@vars x
ex = cos(sin(x) + sin(2x))
replace(ex, sin(a), sin(2a))
```


"""
function Base.replace(ex::Sym, query::Sym, fn::Function; kwargs...)
    ## XXX this is failing!
    replace(ex, query, PyCall.PyObject((args...) ->fn(args...)); kwargs...)
end

function Base.replace(ex::Sym, query::Sym, value; kwargs...)
    ex.replace(query, value; kwargs...)
end
## curried form
Base.replace(query::Sym, value; kwargs...) = ex -> replace(ex, query, value; kwargs...)



"""
    `rewrite(ex, f)`, `rewrite(ex, f, g)` rewrite named functions in an expression by others

Examples:
```
rewrite(sin(x), func(exp(x)))  # rewrite interms of exponentials
rewrite(sin(x), "exp")         # can use a string here
```
"""
function rewrite(ex::Sym,  args...; kwargs...)
    ex.rewrite(args...; kwargs...)
end
export rewrite



"""

    `xreplace(ex, rule::Dict)` or `xreplace(ex, a=>b, c=>d, ...)`

Replace expressions in `ex` using replacements specified in a dictionary or as pairs.

```
x,y, z = symbols("x y z")
xreplace(x*y + x, x*y => PI)
xreplace(x*y*z, x*y => PI)   # entire node of expression tree is not matched
xreplace(2x, 2x => y, x => z)
xreplace(2*2*x, 2x=>y, x => z)
xreplace(Integral(sin(x), x), x=> y) # bound and unbound are replaced
```
"""
function xreplace(ex::Sym, rule::Dict, args...; kwargs...)
    ex.xreplace(rule, args...; kwargs...)
end
xreplace(ex::Sym, xs::Pair...; kwargs...) = xreplace(ex, Dict(xs...); kwargs...)

export xreplace
