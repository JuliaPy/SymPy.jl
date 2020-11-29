# SymPy.jl

Documentation for [SymPy.jl](https://github.com/JuliaPy/SymPy.jl) a `Julia` interface to Python's [SymPy](https://www.sympy.org/en/index.html) library for symbolic mathematics.

```@index
Pages  = ["index.md", "introduction.md", "Tutorial/index.md"]
Depth = 1
```	


To install the package, run

```julia
(v1.4) pkg> add SymPy
```

This package relies on `PyCall` to provide a link between `Julia` and `Python`. Installation should  install `PyCall`, the `sympy` module for python, and if needed, a python executable.



##  Quick example

This  package  provides a convenient  way  to access the functionality of the  `sympy` library for `Python` from `Julia`, utilizing  the  `PyCall` package to provide the interface. A symbolic type, a wrapper  around a `PyCall.PyObject`, is provided  and, as  much as reasonable,  generic `Julia` methods are created for this type. Many sympy  specific features  are available through a qualified function call, e.g. `sympy.funcname(...)` or a `Python` method call,  e.g., `obj.methname(...)`.

This example from calculus provides an illustation of each.

```jldoctest index
julia> using SymPy
```

Here we create some symbolic variables, one with an assumption:

```jldoctest index
julia> @vars x
(x,)

julia> @vars a real=true
(a,)
```

The basic math functions have methods for symbolic expressions:

```jldoctest index
julia> sin(x)
sin(x)

julia> log(x)
log(x)
```

The output is a symbolic  expression.


A selection of SymPy functions are defined as `Julia` functions. Here are three common calculus  operations:

```jldoctest index
julia> limit(sin(a*x)/x, x =>  0)
a

julia> diff(x^x, (x,3))
 x ⎛            3   3⋅(log(x) + 1)   1 ⎞
x ⋅⎜(log(x) + 1)  + ────────────── - ──⎟
   ⎜                      x           2⎟
   ⎝                                 x ⎠

julia> integrate(exp(-a*x) * sin(x), x)
     a⋅sin(x)          cos(x)
- ────────────── - ──────────────
   2  a⋅x    a⋅x    2  a⋅x    a⋅x
  a ⋅ℯ    + ℯ      a ⋅ℯ    + ℯ   
```

Both  `limit`  and `integrate` are defined within  the package,  whereas `diff`, a generic  `Julia`  function, has a method defined for the  first argument being symbolic.


The  `diff` function finds  derivatives, SymPy's `Derivative` function defines *unevaluated* derivatives, which are evaluated through their  `doit` method. `Derivative` is not a `Julia`  function, so we must  qualify  it:

```jldoctest index
julia> out = sympy.Derivative(x^x, x)
d ⎛ x⎞
──⎝x ⎠
dx    

julia> out.doit()
 x
x ⋅(log(x) + 1)
```

## Using other features of SymPy

By design, along with methods defined for generic functions in `Julia`, only a select number of core SymPy functions are exported; others need to be qualified. Many can be found, as above, from the syntax `sympy.XXX`, where the `XXX` method from the underlying `sympy` module is used. Many more must be imported before being available. 

For example, the [Stats](https://docs.sympy.org/latest/modules/stats.html) module provides methods to support the concept of a random variable used in probability and statistics. The following shows how to import all the methods into a session:

```jldoctest Stats
julia> SymPy.PyCall.pyimport_conda("sympy.stats", "sympy")
PyObject <module 'sympy.stats' from '/Users/verzani/.julia/conda/3/lib/python3.7/site-packages/sympy/stats/__init__.py'>

julia> SymPy.import_from(sympy.stats)

julia> p = 1//2
1//2

julia> @vars x integer=true positive=true
(x,)

julia> pdf = p * (1-p)^(x-1);

julia> D = DiscreteRV(x, pdf, set=sympy.S.Naturals)
x

julia> E(D)
2

julia> P(D ≫ 3)
1/8
```

The `import_from` function imports all the functions it can, creating methods specialized on their first argument being symbolic. In this case, `E` and `P` are used above without qualification. `Naturals`, above, is in a different sympy module, and hasn't been imported, so it must be qualified above. The `pyimport_conda` call of `PyCall` will import the module into the specific name, and if necessary install the underlying package.

The above works well for interactive usage, but would cause problems were it included in package code, as the assignment within `pyimport_conda` occurs at run time. There are necessary workarounds.
