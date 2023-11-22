# SymPy

[![Docs](https://img.shields.io/badge/docs-dev-blue.svg)](https://jverzani.github.io/SymPyCore.jl/dev)
[![Build Status](https://github.com/jverzani/SymPy.jl/actions/workflows/CI.yml/badge.svg?branch=main)](https://github.com/jverzani/SymPy.jl/actions/workflows/CI.yml?query=branch%3Amain)
[![Coverage](https://codecov.io/gh/jverzani/SymPy.jl/branch/main/graph/badge.svg)](https://codecov.io/gh/jverzani/SymPy.jl)


[SymPyCore](https://github.com/jverzani/SymPyCore.jl) provides a `Julia`n interface to the [SymPy](https://www.sympy.org/) library of Python.


`SymPy`  utilizes `SymPyCore` and the `PyCall` package (to provide the interop between `Julia` and `Python`) to enable access to Python's SymPy library using the practices and idioms of `Julia`.

The package [SymPyPythonCall](https://github.com/jverzani/SymPyPythonCall.jl) does a similar thing with the `PythonCall` package providing the interop.

### Installation

To use this package, both Python and its SymPy library must be
installed on your system. If `PyCall` is installed using `Conda`
(which is the default if no system `python` is found), then the
underlying SymPy library will be installed via `Conda` when the
package is first loaded. Otherwise, installing both Python and the
SymPy library can be done by other means.
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
