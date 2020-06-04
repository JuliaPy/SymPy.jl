# SymPy.jl



Documentation for [SymPy.jl](https://github.com/JuliaPy/SymPy.jl) a `Julia` interface to Python's [SymPy](https://github.com/JuliaPy/SymPy.jl) library for symbolic mathematics.

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

