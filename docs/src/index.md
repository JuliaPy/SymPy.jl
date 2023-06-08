# SymPy.jl

The `Julia` package [SymPy](https://github.com/JuliaPy/SymPy.jl) uses the `PyCall` package to interface `Julia` with the underlying SymPy Python package. SymPy ([http://sympy.org/](http://sympy.org/)) is a Python library for symbolic mathematics. The [Symbolics](https://symbolics.juliasymbolics.org/) package for `Julia` provides an alternative with some compelling performance improvements, though is not nearly as feature rich as the underlying SymPy Python library.

## Using Python's SymPy with `PyCall`

The `PyCall` package does the heavy lifting for `SymPy`. In fact, this package can be skipped altogether, if so inclined. For example, we can import the underlying Python module as follows:

```jldoctest sympy
julia> using PyCall

julia> sympy = pyimport("sympy")
[...]
```

Using the dot-call notation of Python, we can create a symbolic variable, and a symbolic expression:

```jldoctest sympy
julia> x = sympy.symbols("x")
PyObject x

julia> y = sympy.sin(x)
PyObject sin(x)
```

The symbolic expression can be evaluated at a symbolic value returning an object accessible via pycall. In the following, this is then converted to a floating point number in `Julia`:

```jldoctest sympy
julia> z = y.subs(x, sympy.pi)
PyObject 0

julia> convert(Float64, z)
0.0
```

Further, some arithmetic operations are already available, for example multiplication and powers:

```{julia}
julia> y = sympy.sin(sympy.pi * x)^2 # ^ in Julia, ** in Python
PyObject sin(pi*x)**2

julia> z = y.subs(x,2)
PyObject 0
```

## Using SymPy with `SymPy`

The `SymPy` package provides a more Julian interface to such tasks;

```jldoctest sympy
julia> using SymPy
WARNING: using SymPy.sympy in module Main conflicts with an existing identifier.

julia> @syms x
(x,)

julia> ex = sin(pi*x)^2     # generic method overload
   2
sin (π⋅x)

julia> z = ex(x => 1) # Julia style, use ex.subs((x, PI)) for Python style
0

julia> N(z)            # z refers to a python value; N converts this symbolic number to a number in Julia
0
```

As seen in the warning, the `SymPy` package also exposes the underlying `sympy` module. In addition, the basic usage follows these points:

* generic methods from `Julia` and imported functions in the `sympy` namespace are called through `fn(object)`. Method overloading for basic generics usually expects the first argument to be symbolic for dispatch.

* SymPy methods of an object are called through Python's dot-call syntax: `object.fn(...)`.

* Constructors, like `sympy.Symbol`, and other non-function calls from `sympy` are qualified with `sympy.Constructor(...)`. Such qualified calls are also useful when the first argument is not symbolic.


## Installation

This package can be installed through the usual means (`using SymPy` or `add SymPy`).

Installation of the package will also install `PyCall`, if needed; a `python` interpreter, if needed; and then use the `Conda` package to install the underlying SymPy library, if needed.

For the `Conda` installation, the following commands can be used to update the underlying python library:

```
using Conda  # may need to be installed
Conda.update()
```

!!! warning
    If installation does not work, there may be a conflict with another `python` installation. There have been reports for Mac OS systems that a Conda installed python and a brew installed python do not work well together, with the solution being to delete the brew installed version.
