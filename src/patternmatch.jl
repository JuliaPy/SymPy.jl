## Pattern matching modifications


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

The order of the arguments follows `Julia`'s `match` function, not `sympy.match`, which can be used directly, otherwise.
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

* "types" are specified via calling `func` on the head of an expression: `func(sin(x))` -> `sin`, or directly through `sympy.sin`

* functions are supported, but only with `PyCall` commands.


Examples (from the SymPy docs)

```
x, y, z = symbols("x, y, z")
f = log(sin(x)) + tan(sin(x^2))
```

## "type" -> "type"

Types are specified through `func`:
```
func = SymPy.Introspection.func
replace(f, func(sin(x)), func(cos(x))) # log(cos(x)) + tan(cos(x^2))  # type -> type
```

The `func` function finds the head of an expression (`sin` and `cos` above). This could also have been written (perhaps more directly) as:

```
replace(f, sympy.sin, sympy.cos)
```

## "type" -> "function"

To replace with a more complicated function, requires some assistance from `Python`, as an anonymous function must be defined witin Python, not `Julia`:

```
import PyCall
## Anonymous function a -> sin(2a)
PyCall.py\"\"\"
from sympy import sin, Mul
def anonfn(*args):
    return sin(2*Mul(*args))
\"\"\"

replace(f, sympy.sin, PyCall.py"anonfn")
```

## "pattern" -> "expression"

Using "`Wild`" variables allows a pattern to be replaced by an expression:

```
a, b = Wild("a"), Wild("b")
replace(f, sin(a), sin(2a))
```

In the SymPy docs we have:

Matching is exact by default when more than one Wild symbol is used: matching fails unless the match gives non-zero values for all Wild symbols."

```
replace(2x + y, a*x+b, b-a)  # y - 2
replace(2x + y, a*x+b, b-a, exact=false)  # y + 2/x
```

## "pattern" -> "func"

The function is redefined, as a fixed argument is passed:

```
PyCall.py\"\"\"
from sympy import sin
def anonfn(a):
    return sin(2*a)
\"\"\"
replace(f, sin(a), PyCall.py"anonfn")
```

## "func" -> "func"

```
PyCall.py\"\"\"
def fn1(expr):
    return expr.is_Number

def fn2(expr):
    return expr**2
\"\"\"
replace(2*sin(x^3), PyCall.py"fn1", PyCall.py"fn2")
```

```
PyCall.py\"\"\"
def fn1(x):
    return x.is_Mul

def fn2(x):
    return 2*x
\"\"\"
replace(x*(x*y + 1), PyCall.py"fn1", PyCall.py"fn2")
```
"""
function Base.replace(ex::Sym, query::Sym, fn::Function; exact=true, kwargs...)
    ## XXX this is failing!
    ex.replace(query, PyCall.PyObject((args...) ->fn(args...)); exact=exact, kwargs...)
end


function Base.replace(ex::Sym, query::Any, value; exact=true, kwargs...)
    ex.replace(query, value; exact=exact, kwargs...)
end
