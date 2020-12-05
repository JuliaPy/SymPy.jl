[![Build status](https://github.com/JuliaPy/SymPy.jl/workflows/CI/badge.svg)](https://github.com/JuliaPy/SymPy.jl/actions)
&nbsp;
[![](https://img.shields.io/badge/docs-stable-blue.svg)](https://juliahub.com/docs/SymPy)



# SymPy Package to bring Python's `Sympy` functionality into `Julia` via `PyCall`



SymPy  (`http://sympy.org/`)  is a Python library for symbolic mathematics.

With the excellent `PyCall` package of `julia`, one has access to the
many features of the SymPy library from within a `Julia` session.

This `SymPy` package provides a light interface for  the
features of the SymPy library that makes working with SymPy objects a bit
easier.

The documentation inludes an introduction document and a version of
the SymPy tutorial translated from the Python syntax into Julia.

### Installation

To use this package, both Python and its SymPy library must be
installed on your system. If `PyCall` is installed using `Conda`
(which is the default if no system `python` is found), then the
underlying SymPy library will be installed via `Conda` when the
package is first loaded. Otherwise, installing both Python and the
SymPy library (which also requires mpmath) can be done by other means.
In this case, the `Anaconda` distribution is suggested, as it provides a single
installation of Python that includes SymPy and many other
scientific libraries that can be profitably accessed within `Julia`
via `PyCall`. (Otherwise, install Python then download the SymPy
library from https://github.com/sympy/sympy/releases and install.)

To upgrade the underlying `sympy` library, which has new releases at a
rate similar to `Julia`, when installed with `Conda`, the following commands
are available:

```
using Pkg
Pkg.add("Conda") #  if needed
using Conda
Conda.update()
```

## The `PyCall` interface to `SymPy`

The only point to this package is that using `PyCall` to access
SymPy is somewhat cumbersome. The following is how one would define
a symbolic value `x`, take its sine, then evaluate the symboic
expression for `x` equal `pi`, say:

```
using PyCall
sympy = pyimport("sympy")  #
x = sympy.Symbol("x")      # PyObject x
y = sympy.sin(x)           # PyObject sin(x)
z = y.subs(x, sympy.pi)    # PyObject 0
convert(Float64, z)        # 0.0
```


The `sympy` object imported on the second line provides the access to
much of SymPy's functionality, allowing access to functions
(`sympy.sin`), properties, modules (`sympy`), and classes
(`sympy.Symbol`, `sympy.Pi`).  The `Symbol` and `sin` operations are found
within the imported `sympy` module and, as seen, are referenced with
`Python`'s dot call syntax, as implemented in `PyCall` through a
specialized `getproperty` method.

SymPy's functionality is also found through methods bound to
an object of a certain class. The `subs` method of the `y` object is an
example. Such methods are also accessed with Python's dot-call
syntax. The call above substitutes a value of `sympy.pi` for the
symbolic variable `x`. This leaves the object as a `PyObject` storing
a number which can be brought back into `julia` through conversion, in
this case through an explicit `convert` call.


Alternatively, `PyCall` now has a `*` method, so the above could also be done with:

```
x = sympy.Symbol("x")
y = sympy.pi * x
z = sympy.sin(y)
convert(Float64, z.subs(x, 1))
```

With the `SymPy` package this gets replaced by a more `julia`n syntax:

```
using SymPy
x = symbols("x")		       # or   @vars x, Sym("x"), or  Sym(:x)
y = sin(pi*x)
y(1)                           # Does y.subs(x, 1). Use y(x=>1) to be specific as to which symbol to substitute
```

The object `x` we create is of type `Sym`, a simple proxy for the
underlying `PyObject`. The package overloads the familiar math functions so
that working with symbolic expressions can use natural `julia`
idioms. The final result  here is a symbolic value of `0`, which
prints as `0` and not `PyObject 0`. To convert it into a numeric value
within `Julia`, the `N` function may be used, which acts like the
float conversion, only there is an attempt to preserve the variable type.

(There is a subtlety, the value of `pi` here (an `Irrational` in
`Julia`) is converted to the symbolic `PI`, but in general won't be if
the math constant is coerced to a floating point value before it
encounters a symbolic object. It is better to just use the symbolic
value `PI`, an alias for `sympy.pi` used above.)


----


SymPy has a mix of function calls (as in `sin(x)`) and method calls
(as in `y.subs(x,1)`). The function calls are from objects in the base
`sympy` module. When the `SymPy` package is loaded, in addition to
specialized methods for many generic `Julia` functions, such as `sin`,
a priviledged set of the function calls in `sympy` are imported as
generic functions narrowed on their first argument being a symbolic
object, as constructed by `Sym` or `symbols`. (Calling
`import_from(sympy)` will import all the function calls.)

The basic usage follows these points:

* generic methods from `Julia` and imported functions in the `sympy`
  namespace are called through `fn(object)`

* SymPy methods are called through Python's dot-call syntax:
  `object.fn(...)`

* Contructors, like `sympy.Symbol`, and other non-function calls from `sympy` are qualified
  with `sympy.Constructor(...)`. Such qualified calls are also useful
  when the first argument is not symbolic.


So, these three calls are different,

```
sin(1), sin(Sym(1)), sympy.sin(1)
```

The first involves no symbolic values. The second and third are
related and return a symbolic value for `sin(1)`. The second
dispatches on the symbolic argument `Sym(1)`, the third has no
dispatch, but refers to a SymPy function from the `sympy` object. Its
argument, `1`, is converted by `PyCall` into a Python object for the
function to process.

In the initial example, slightly rewritten, we could have written:

```
x = symbols("x")
y = sin(pi*x)
y.subs(x, 1)
```

The first line calls a provided alias for `sympy.symbols` which is
defined to allow a string (or a symbol) as an argument. The second,
dispatches to `sympy.sin`, as `pi*x` is symbolic-- `x` is, and
multiplication promotes to a symbolic value. The third line uses the
dot-call syntax of `PyCall` to call the `subs` method of the symbolic
`y` object.


Not illustrated above, but classes and other objects from SymPy are
not brought in by default, and can be accessed using qualification, as
in `sympy.Function` (used to define symbolic functions).
