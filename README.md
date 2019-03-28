[![SymPy](http://pkg.julialang.org/badges/SymPy_0.7.svg)](http://pkg.julialang.org/?pkg=SymPy&ver=0.7)

Linux: [![Build Status](https://travis-ci.org/JuliaPy/SymPy.jl.svg?branch=master)](https://travis-ci.org/JuliaPy/SymPy.jl)
&nbsp;
Windows: [![Build Status](https://ci.appveyor.com/api/projects/status/github/JuliaPy/SymPy.jl?branch=master&svg=true)](https://ci.appveyor.com/project/jverzani/sympy-jl)



# SymPy Package to bring Python's `Sympy` functionality into `Julia` via `PyCall`



The `SymPy` package  (`http://sympy.org/`)  is a Python library for symbolic mathematics.

With the excellent `PyCall` package of `julia`, one has access to the
many features of `SymPy` from within a `Julia` session.

This `SymPy` package provides a light interface for _some_ of the
features of `SymPy` that makes working with `SymPy` objects a bit
easier.

The [tutorial](examples/tutorial.md) provides an overview. It is
viewable as an `IJulia` notebook
[here](http://nbviewer.ipython.org/github/jverzani/SymPy.jl/blob/sympylite/examples/tutorial.ipynb). In addition, most of the SymPy tutorial has the `julia` counterparts illustrated starting from [index.html](http://htmlpreview.github.io/?https://github.com/jverzani/SymPy.jl/blob/sympylite/examples/index.html)

### Installation

To use this package, both `Python` and its `SymPy` library must be
installed on your system. If `PyCall` is installed using `Conda`
(which is the default if no system `python` is found), then the
underlying `SymPy` library will be installed via `Conda` when the
package is first loaded. Otherwise, installing both `Python` and
`SymPy` (which also requires `mpmath`) can be done by other means.
In this case, the `Anaconda` distribution is suggested, as it provides a single
installation of `Python` that includes `SymPy` and many other
scientific libraries that can be profitably accessed within `Julia`
via `PyCall`. (Otherwise, install `Python` then download the `sympy`
library from https://github.com/sympy/sympy/releases and install.)

## The `PyCall` interface to `SymPy`

The only point to this package is that using `PyCall` to access
`SymPy` is somewhat cumbersome. The following is how one would define
a symbolic value `x`, take its sine, then evaluate at `pi`, say:

```
using PyCall
sympy = pyimport("sympy")
x = sympy.Symbol("x")
y = sympy.sin(x)
z = y.subs(x, sympy.pi)
convert(Float64, z)
```

The `Symbol` and `sin` function of `SymPy` are found within the
imported `sympy` object. They may be referenced with `Python`'s dot
notation. Similarly, the `subs` method of the `y` object may be
accessed with Python's dot nottation using PyCall's `getproperty`
overload to call the method. The call above substitutes a value of
`sympy.pi` for `x`. This leaves the object as a `PyObject` storing a
number which can be brought back into `julia` through conversion, in
this case through an explicit `convert` call.


Alternatively, `PyCall` now has a `*` method, so this could be done with

```
x = sympy.Symbol("x")
y = sympy.pi * x
z = sympy.sin(y)
convert(Float64, z.subs(x, 1))
```

With `SymPy` this gets replaced by a more `julia`n syntax:

```
using SymPy
x = symbols("x")		       # or   @vars x, Sym("x"), or  Sym(:x)
y = sin(pi*x)
y(1)                           # Does y.subs(x, 1). Use y(x=>1) to be specific as to which symbol to substitute
```

The object `x` we create is of type `Sym`, a simple proxy for the
underlying `PyObject`. We then overload the familiar math functions so
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


SymPy has a mix of function calls (as in `sin(x)`) and method calls (as in `y.subs(x,1)`). The function calls are from objects in the base `sympy` class. When `SymPy` is loaded, the function calls in `sympy` are imported as generic functions narrowed on their first argument being a symbolic object, as constructed by `Sym` or `sympify`. The basic usage follows these points:

* generic methods from `Julia` and functions in the `sympy` namespace are called through `fn(object)`

* SymPy methods are called through Python's dot-call syntax: `object.fn(...)`

* Contructors and other non-function calls from `sympy` are qualified with `sympy.Constructor(...)`. Such qualified calls are also useful when the first argument is not symbolic.=


So, these three calls are different,

```
sin(1), sin(Sym(1)), sympy.sin(1)
```

The first involves no symbolic values. The second and third are related and return a symbolic value for `sin(1)`. The second dispatches on the symbolic argument `Sym(1)`, the third has no dispatch, but refers to a SymPy function from the `sympy` object. Its argument, `1`, is converted by `PyCall` into a Python object for the function to process.

In the initial example, slightly rewritten, we could have written:

```
x = symbols("x")
y = sin(pi*x)
y.subs(x, 1)
```

The first line calls a provided alias for `sympy.symbols` which is defined to allow a string (or a symbol) as an argument. The second, dispatches to `sympy.sin`, as `pi*x` is symbolic, since `x` is and multiplication promotes to a symbolic value. The third line uses the dot-call syntax of `PyCall` to call the `subs` method of the symbolic `y` object.


Not illustrated above, but classes and other objects from SymPy are
not brought in by default, and can be accessed using qualification, as
in `sympy.Function` (used to define symbolic functions).
