# The SymPy tutorial (1.3) in `Julia`

The `SymPy` package for `Julia` allows julia users to interact with  python's SymPy module in a mostly seamless manner, thanks to the power of the `PyCall` package for `Julia`. The following pages reexpress the SymPy tutorial illustrating the associated `Julia` commands. There are some changes, but mostly modest ones.

The `SymPy` package for `Julia` essentially does:

* it provides a type for symbolic objects, a simple wrapper around the underlying `PyCall` objects.

Such objects may be created using the SymPy syntax:

```
using SymPy
x, y = symbols("x, y")
```

Alternatively, the `@vars` macro may be used:

```
@vars a b
```

* Symbolic objects are subtypes of numbers, `SymPy` provides methods for the generic interface for Julia number. For example, elementary operations `+`, `-`, `*`, `/`, `^` are defined as well as more `Julia`-specific methodssuch as `ones`, `zeros`, `eps`, etc.

* SymPy is organized around various modules, from which functions are imported for use. A common SymPy command would be:

```verbatim
from sympy import *
```

This imports all functions *and* modules from the base sympy module into the user's namespace. By default, `SymPy` imports all functions from `sympy` -- but *not* modules, as there would be several conflicts with fundamental `Julia` objects (e.g., `Matrix`). As such, modules must be qualified, as in `sympy.Matrix`. (`SymPy` exports the underlying `sympy` object.) For other modules, `SymPy` provides the function `import_from` and `import_from_all` to load or bulk load all functions from a given module.

The functions imported by `SymPy` are specialized so that their first argument is of symbolic type. This is not always appropriate, as some SymPy functions naturally take integer values for the first entry. An example is `ones`. Either the integer can be made symbolic (as with `Sym(1)`) or the function can be qualified, as in `sympy.ones`.

* SymPy and `Julia` have matrix (and vector) objects. These may be expressed as elements of type `Matrix{Sym}` (using `Julia`'s generic matrix type to store symbolic objects, or as elements of `SymMatrix` (a simple wrapper for an underlying python object). SymPy methods for matrices are available using the `getproperty` notation (the dot), whereas, `Julia` methods would just use the fact that `Matrix{Sym}` will have many generic methods already defined for it.

A quick example:

```

@vars x
A = [1 x; x x^3]  # Matrix{Sym}
M = sympy.Matrix([[1, x], [x, x^3]])  # SymMatrix
using LinearAlgebra
M.det(), det(lu(A, Val(false)))
```

The latter determinant is more work to find, as a non-pivoting `lu` method needs to be called for symbolic objects with free symbols.


## Table of contents

* [Introduction](intro.html)

* [Gotchas](gotchas.html)

* [Basic Operations](basic_operations.html)

* [Simplification](simplification.html)

* [Calculus](calculus.html)

* [Solvers](solvers.html)

* [Matrices](matrices.html)

* [Advanced Expression Manipulation](manipulation.html)
