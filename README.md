[![SymPy](http://pkg.julialang.org/badges/SymPy_0.4.svg)](http://pkg.julialang.org/?pkg=SymPy&ver=0.4)
&nbsp;
[![SymPy](http://pkg.julialang.org/badges/SymPy_0.5.svg)](http://pkg.julialang.org/?pkg=SymPy&ver=0.5)
&nbsp;
[![SymPy](http://pkg.julialang.org/badges/SymPy_0.6.svg)](http://pkg.julialang.org/?pkg=SymPy&ver=0.6) 

Linux: [![Build Status](https://travis-ci.org/JuliaPy/SymPy.jl.svg?branch=master)](https://travis-ci.org/JuliaPy/SymPy.jl)
&nbsp;
Windows: [![Build Status](https://ci.appveyor.com/api/projects/status/github/JuliaPy/SymPy.jl?branch=master&svg=true)](https://ci.appveyor.com/project/jverzani/sympy-jl)



# SymPy Package to bring Python's `Sympy` functionality into `Julia` via `PyCall`



The `SymPy` package  (`http://sympy.org/`)  is a Python library for symbolic mathematics. 

With the excellent `PyCall` package of `julia`, one has access to the
many features of `SymPy` from a `julia` session.

This `SymPy` package provides a light interface for _some_ of the
features of `SymPy` that makes working with `SymPy` objects a bit
easier.

The [tutorial](examples/tutorial.md) provides an overview. It is
viewable as an `IJulia` notebook
[here](http://nbviewer.ipython.org/github/jverzani/SymPy.jl/blob/master/examples/tutorial.ipynb).

### Installation

To use this package, both `Python` and its `SymPy` library must be
installed on your system. If `PyCall` is installed using `Conda`
(which is the default if no system `python` is found), then the
underlying `SymPy` library will be installed via `Conda` when the
package is first loaded. Otherwise, installing both `Python` and
`SymPy` (which also requires `mpmath`) can be done by other means.
The `Anaconda` distribution is suggested, as it provides a single
installation of `Python` that includes `SymPy` and many other
scientifice libraries that can be profitably accessed within `Julia`
via `PyCall`. (Otherwise, install `Python` then download the `sympy`
library from https://github.com/sympy/sympy/releases and install.)

## The `PyCall` interface to `SymPy`

The only point to this package is that using `PyCall` to access
`SymPy` is somewhat cumbersome. The following is how one would define
a symbolic value `x`, take its sine, then evaluate at `pi`, say:

```
using PyCall			
@pyimport sympy
x = sympy.Symbol("x")
y = sympy.sin(x)
y[:subs](x, sympy.pi) |> float
```

The `Symbol` and `sin` function of `SymPy` are found within the
imported `sympy` object. They may be referenced with `Python`'s dot
notation. However, the `subs` method of the `y` object is accessed
differently, using indexing notation with a symbol. The call above
substitutes a value of `sympy.pi` for `x`. This leaves the object as a
`PyObject` storing a number which can be brought back into `julia`
through conversion, in this case with the `float` function.

The above isn't so awkward, but even more cumbersome is the similarly
simple task of finding `sin(pi*x)`.  As this multiplication is done at
the python level and is not a method of `sympy` or the `x` object, we
need to evaluate python code. Here is one solution:

```
x = sympy.Symbol("x")
y = pycall(sympy.Mul, PyAny, sympy.pi, x)
z = sympy.sin(y)		
z[:subs](x, 1) |> float
```

This gets replaced by a more `julia`n syntax:

```
using SymPy                    
x = symbols("x")		       # or   @syms x, Sym("x"), or  Sym(:x)
y = sin(pi*x)
y(1)                           # Does subs(y, x, 1). Use y(x=>1) to be specific as to which symbol to substitute
```

The object `x` we create is of type `Sym`, a simple proxy for the
underlying `PyObject`. We then overload the familiar math functions so
that working with symbolic expressions can use natural `julia`
idioms. The final result is here is a symbolic value of `0`, which
prints as `0` and not `PyObject 0`. To convert it into a numeric value
within `Julia`, the `N` function may be used, which acts like the
`float` call, only attempts to preserve the variable type.

(There is a subtlety, the value of `pi` here is converted to the
symbolic `PI`, but in general won't be if the math constant is coerced
to a floating point value before it encounters a symbolic object. It
is better to just use the symbolic value `PI`, an alias for `sympy.pi`
used above. A similar comment applies for `e`, where `E` should be
used.)

However, for some tasks the `PyCall` interface is still needed, as
only a portion of the `SymPy` interface is exposed. To call an
underlying SymPy method, the `getindex` method is overloaded for
`symbol` indices so that `ex[:meth_name](...)` dispatches to either to
SymPy's `ex.meth_name(...)` or `meth_name(ex, ...)`, as possible.
There is a `sympy` string macro to simplify this a bit, with the call
looking like: `sympy"meth_name"(...)`, for example `sympy"harmonic"(10)`.


## Notes

Some aspects of `SymPy` require more modern versions of `sympy` to be
installed. For example, the matrix functions rely on features of
`sympy` that are not exposed in the `sympy` installed with Ubuntu LTS
12.04.

In that particular instance, calls such as

```
x = symbols("x")
a = [x 1; 1 x]
det(a)
```

Can be replaced with

```
sympy"det"(a)
```

Similarly for `trace`, `eigenvects`, ... . Note these are `sympy`
methods, not `Julia` methods that have been ported. (Hence,
`eigenvects` and not `eigvecs`.)





