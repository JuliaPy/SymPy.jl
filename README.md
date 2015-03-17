## Package to bring `sympy` functionality into `julia` via `PyCall`

The `SymPy` package  (`http://sympy.org/`)  is a Python library for symbolic mathematics. 

With the excellent `PyCall` package of `julia`, one has access to the
many features of `SymPy` from a `julia` session.

This `SymPy` package provides a light interface for _some_ of the
features of `SymPy` that makes working with `SymPy` objects a bit
easier. The [tutorial](examples/tutorial.md) provides an overview.

### Installation

To use it, both `Python` and the `SymPy` package must be installed on
your system. The `Anaconda` distribution is suggested, as it includes
much more than `SymPy` that can be profitably accessed within `Julia`
via `PyCall`. (Otherwise, install `Python` then download the `sympy`
library from http://code.google.com/p/sympy/downloads/list and
install.)

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
y = pyeval("k*x", k=sympy.pi, x=x)     
z = sympy.sin(y)		
z[:subs](x, 1) |> float		
```

This gets replaced by a more `julia`n syntax:

```
using SymPy                     # some warnings need cleaning up
x = Sym("x")		            # or  Sym(:x), symbols("x"), or (x,) = @syms x
y = sin(pi*x)                    
subs(y, x, 1) |> float
```

The object `x` we create is of type `Sym`, a simple proxy for the
underlying `PyObject`. We then overload the familiar math functions so
that working with symbolic expressions can use natural `julia` idioms.

However, the `PyCall` interface is needed for serious work, as only a
small portion of the `SymPy` interface is exposed.  To dig the
`PyObject` out of a `Sym` object, you access its property `x`, as in
`y.x`. This is useful if passing a `Sym` object to a method call,
though `getindex` is overridden for `Sym` objects and symbol indices
to call the method.

## Notes

Some aspects of `SymPy` require more modern versions of `sympy` to be
installed. For example, the matrix functions rely on features of
`sympy` that are not exposed in the `sympy` installed with Ubuntu LTS
12.04.

In that particular instance, calls such as

```
x = Sym("x")
a = [x 1; 1 x]
det(a)
```

Can be replaced with

```
a[:det]()
```

Similarly for `:trace`, `:eigenvects`, ... . Note these are `sympy`
methods, not `Julia` methods that have been ported. (Hence,
`:eigenvects` and not `eigvecs`.)

## TODO

- Try `@doc` for documentation links back to SymPy's documentation.



