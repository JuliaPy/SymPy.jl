# Solvers

[From](https://docs.sympy.org/latest/tutorial/solvers.html)


```python
    >>> from sympy import *
    >>> x, y, z = symbols("x y z")
    >>> init_printing(use_unicode=True)
```


```@setup solvers
using SymPy
sympy.init_printing(use_unicode=True)
```

##### In `Julia`:

```jldoctest solvers
julia> using SymPy

julia> @syms x, y, z
(x, y, z)
```

----

## A Note about Equations


Recall from the :ref:`gotchas <tutorial_gotchas_equals>` section of this
tutorial that symbolic equations in SymPy are not represented by `=` or
`==`, but by `Eq`.


```python
    >>> Eq(x, y)
    x = y
```

##### In `Julia`:

```jldoctest solvers
julia> Eq(x, y)
x = y

```

----

However, there is an even easier way.  In SymPy, any expression not in an
`Eq` is automatically assumed to equal 0 by the solving functions.  Since `a
= b` if and only if `a - b = 0`, this means that instead of using `x == y`,
you can just use `x - y`.  For example

```python
    >>> solveset(Eq(x**2, 1), x)
    {-1, 1}
    >>> solveset(Eq(x**2 - 1, 0), x)
    {-1, 1}
    >>> solveset(x**2 - 1, x)
    {-1, 1}
```

##### In `Julia`:

```jldoctest solvers
julia> solveset(Eq(x^2, 1), x)
{-1, 1}

julia> solveset(Eq(x^2 - 1, 0), x)
{-1, 1}

julia> solveset(x^2 - 1, x)
{-1, 1}
```

----

This is particularly useful if the equation you wish to solve is already equal
to 0. Instead of typing `solveset(Eq(expr, 0), x)`, you can just use
`solveset(expr, x)`.

## Solving Equations Algebraically


The main function for solving algebraic equations is `solveset`.
The syntax for `solveset` is `solveset(equation, variable=None, domain=S.Complexes)`
Where `equations` may be in the form of `Eq` instances or expressions
that are assumed to be equal to zero.

Please note that there is another function called `solve` which
can also be used to solve equations. The syntax is `solve(equations, variables)`
However, it is recommended to use `solveset` instead.

When solving a single equation, the output of `solveset` is a `FiniteSet` or
an `Interval` or `ImageSet` of the solutions.

```python
    >>> solveset(x**2 - x, x)
    {0, 1}
    >>> solveset(x - x, x, domain=S.Reals)
    ℝ
    >>> solveset(sin(x) - 1, x, domain=S.Reals)
    ⎧        π        ⎫
    ⎨2⋅n⋅π + ─ | n ∊ ℤ⎬
    ⎩        2        ⎭
```

##### In `Julia`:

* `S` is not exported, as it is not a function, so we create an alias:

```jldoctest solvers
julia> const S = sympy.S
PyObject S

julia> solveset(x^2 - x, x)
{0, 1}

julia> solveset(x - x, x, domain=S.Reals)
ℝ

julia> solveset(sin(x) - 1, x, domain=S.Reals)
⎧        π │      ⎫
⎨2⋅n⋅π + ─ │ n ∊ ℤ⎬
⎩        2 │      ⎭
```

----


If there are no solutions, an `EmptySet` is returned and if it
is not able to find solutions then a `ConditionSet` is returned.

```python
    >>> solveset(exp(x), x)     # No solution exists
    ∅
    >>> solveset(cos(x) - x, x)  # Not able to find solution
    {x | x ∊ ℂ ∧ -x + cos(x) = 0}
```

##### In `Julia`:

```jldoctest solvers
julia> solveset(exp(x), x)     # No solution exists
∅

julia> solveset(cos(x) - x, x)  # Not able to find solution
{x │ x ∊ ℂ ∧ (-x + cos(x) = 0)}
```

----

In the `solveset` module, the linear system of equations is solved using `linsolve`.
In future we would be able to use linsolve directly from `solveset`. Following
is an example of the syntax of `linsolve`.

* List of Equations Form:

```python
    >>> linsolve([x + y + z - 1, x + y + 2*z - 3 ], (x, y, z))
```

##### In `Julia`:

Rather than a vector,  we  pass  a tuple:


```jldoctest solvers
julia> linsolve((x + y + z - 1, x + y + 2*z - 3), (x, y, z))
{(-y - 1, y, 2)}

```

A  tuple
!!! note "Tuples"
    The `linsolve` function  expects a list of equations, whereas  `PyCall`  is instructed to promote the syntax  to   produce  a  list  in  `Python` into a `Array{Sym}` object. As  such, we pass  the equations in a tuple above. Similar considerations  are necessary at times for the  `sympy.Matrix`  constructor. It is suggested, as in the next example, to work around this by passing `Julia`n arrays to the constructor or bypassing it  altogether.


----

* Augmented
Matrix Form:

```python
>>> M = Matrix(((1, 1, 1, 1), (1, 1, 2, 3)))
>>> system = A, b = M[:, :-1], M[:, -1]
>>> linsolve(system, x, y, z)
{(-y - 1, y, 2)}
```

##### In `Julia`:

We use `Julia`n syntax for matrices:

```jldoctest solvers
julia> A =  [1 1 1; 1  1  2];  b =  [1,3]
2-element Vector{Int64}:
 1
 3
```

The augmented form is not available
```
julia> aug = [A b]
2×4 Array{Int64,2}:
 1  1  1  1
 1  1  2  3

julia> linsolve(sympy.Matrix(aug), (x,y,z)) # not {(-y - 1, y, 2)}!
∅

```

In lieu of using `sympy.Matrix`, the matrix can be created  symbolically,   as:

```jldoctest solvers
julia> A =  Sym[1 1 1; 1  1  2];  b =  [1,3]
2-element Vector{Int64}:
 1
 3

julia> aug = [A b]
2×4 Matrix{Sym}:
 1  1  1  1
 1  1  2  3

julia> linsolve(aug, (x,y,z)) # {(-y - 1, y, 2)};

```

Finally,  linear equations  are  solved in `Julia`  with  the `\` (backslash) operator:

```
A \ b
```

The variables are generated  within `\` in  the  sequence  `x1`, `x2`,  ...


----

* A*x = b Form

```python
	>>> M = Matrix(((1, 1, 1, 1), (1, 1, 2, 3)))
	>>> system = A, b = M[:, :-1], M[:, -1]
	>>> linsolve(system, x, y, z)
	{(-y - 1, y, 2)}
```

##### In `Julia`:

We  follow the syntax  above to   construct the matrix (tuple of tuples), but  not  the `Julia`n  matrix  construtor  would be  recommended:

```jldoctest solvers
julia> M = sympy.Matrix(((1, 1, 1, 1), (1, 1, 2, 3)))
2×4 Matrix{Sym}:
 1  1  1  1
 1  1  2  3

julia> system = A, b = M[:, 1:end-1], M[:, end]
(Sym[1 1 1; 1 1 2], Sym[1, 3])

julia> linsolve(system, x, y, z)
{(-y - 1, y, 2)}

```


----

!!! note
    The order of solution corresponds the order of given symbols.

In the `solveset` module, the non linear system of equations is solved using
`nonlinsolve`. Following are examples of `nonlinsolve`.

1. When only real solution is present:

```python
	>>> a, b, c, d = symbols('a, b, c, d', real=True)
	>>> nonlinsolve([a**2 + a, a - b], [a, b])
	{(-1, -1), (0, 0)}
	>>> nonlinsolve([x*y - 1, x - 2], x, y)
	{(2, 1/2)}
```

##### In `Julia`:

* we pass `[a,b]` as either `a, b` or using a tuple, as in `(a,b)`, but *not* as a vector, as this gets mapped into a vector of symbolic objects which causes issues with `nonlinsolve`:

```jldoctest solvers
julia> @syms a::real, b::real, c::real, d::real
(a, b, c, d)

julia> nonlinsolve([a^2 + a, a - b], a, b)
{(-1, -1), (0, 0)}

julia> nonlinsolve([x*y - 1, x - 2], x, y)
{(2, 1/2)}

```

----

2. When only complex solution is present:

```python
	>>> nonlinsolve([x**2 + 1, y**2 + 1], [x, y])
	{(-ⅈ, -ⅈ), (-ⅈ, ⅈ), (ⅈ, -ⅈ), (ⅈ, ⅈ)}
```

##### In `Julia`:

```jldoctest solvers
julia> nonlinsolve([x^2 + 1, y^2 + 1], (x, y))
{(-ⅈ, -ⅈ), (-ⅈ, ⅈ), (ⅈ, -ⅈ), (ⅈ, ⅈ)}

```

----

3. When both real and complex solution is present:

```python
	>>> from sympy import sqrt
	>>> system = [x**2 - 2*y**2 -2, x*y - 2]
	>>> vars = [x, y]
	>>> nonlinsolve(system, vars)
	{(-2, -1), (2, 1), (-√2⋅ⅈ, √2⋅ⅈ), (√2⋅ⅈ, -√2⋅ⅈ)}

	>>> n = Dummy('n')
	>>> system = [exp(x) - sin(y), 1/y - 3]
	>>> real_soln = (log(sin(S(1)/3)), S(1)/3)
	>>> img_lamda = Lambda(n, 2*n*I*pi + Mod(log(sin(S(1)/3)), 2*I*pi))
	>>> complex_soln = (ImageSet(img_lamda, S.Integers), S(1)/3)
	>>> soln = FiniteSet(real_soln, complex_soln)
	>>> nonlinsolve(system, [x, y]) == soln
	True
```

##### In `Julia`:

* we must remove the spaces within `[]`
* we must pass vars as a tuple:

```jldoctest solvers
julia> system = [x^2-2*y^2-2, x*y-2]
2-element Vector{Sym}:
 x^2 - 2*y^2 - 2
         x⋅y - 2

julia> vars = (x, y)
(x, y)

julia> nonlinsolve(system, vars)
{(-2, -1), (2, 1), (-√2⋅ⅈ, √2⋅ⅈ), (√2⋅ⅈ, -√2⋅ⅈ)}

```

However, the next bit requires some modifications to run:

* the `system` array definition must have extra spaces removed
* `Dummy`,  `Mod`, `ImageSet`, `FiniteSet` aren't exported
* we need `PI`, not `pi` to have a symbolic value
* we compare manually

```jldoctest solvers
julia> n = sympy.Dummy("n")
n

julia> system = [exp(x)-sin(y), 1/y-3]
2-element Vector{Sym}:
 exp(x) - sin(y)
        -3 + 1/y

julia> real_soln = (log(sin(S(1)/3)), S(1)/3)
(log(sin(1/3)), 1/3)

julia> img_lamda = Lambda(n, 2*n*IM*PI + sympy.Mod(sin(S(1)/3), 2*IM*PI))
n ↦ 2⋅n⋅ⅈ⋅π + (sin(1/3) mod 2⋅ⅈ⋅π)

julia> complex_soln = (sympy.ImageSet(img_lamda, S.Integers), S(1)/3)
(ImageSet(Lambda(_n, 2*_n*I*pi + Mod(sin(1/3), 2*I*pi)), Integers), 1/3)

julia> soln = sympy.FiniteSet(real_soln, complex_soln)
{(log(sin(1/3)), 1/3), ({2⋅n⋅ⅈ⋅π + (sin(1/3) mod 2⋅ⅈ⋅π) │ n ∊ ℤ}, 1/3)}

julia> nonlinsolve(system, (x, y))
{({2⋅n⋅ⅈ⋅π + log(sin(1/3)) │ n ∊ ℤ}, 1/3)}

```

----

4. If non linear system of equations is Positive dimensional system (A system with
infinitely many solutions is said to be positive-dimensional):

```python
	>>> nonlinsolve([x*y, x*y - x], [x, y])
	{(0, y)}

	>>> system = [a**2 + a*c, a - b]
	>>> nonlinsolve(system, [a, b])
	{(0, 0), (-c, -c)}
```

##### In `Julia`:

* again, we use a tuple for the variables:

```jldoctest solvers
julia> nonlinsolve([x*y, x*y-x], (x, y))
{(0, y)}

```

```jldoctest solvers
julia> system = [a^2+a*c, a-b]
2-element Vector{Sym}:
 a^2 + a*c
     a - b

julia> nonlinsolve(system, (a, b))
{(0, 0), (-c, -c)}

```


----

> Note:

   1. The order of solution corresponds the order of given symbols.

   2. Currently `nonlinsolve` doesn't return solution in form of `LambertW` (if there is solution present in the form of `LambertW`).

   `solve` can be used for such cases:

```python
   >>> solve([x**2 - y**2/exp(x)], [x, y], dict=True)
   ⎡⎧             ⎛y⎞⎫⎤
   ⎢⎨x: 2⋅LambertW⎜─⎟⎬⎥
   ⎣⎩             ⎝2⎠⎭⎦
```

##### In `Julia`:

it is similar

```jldoctest solvers
julia> u = solve([x^2 - y^2/exp(x)], [x, y], dict=true)
2-element Vector{Dict{Any, Any}}:
 Dict(y => -x*sqrt(exp(x)))
 Dict(y => x*sqrt(exp(x)))

```

To get prettier output, the dict may be converted to have one with symbolic keys:

```jldoctest solvers
julia> convert(Dict{SymPy.Sym, Any}, first(u))
Dict{Sym, Any} with 1 entry:
  y => -x*sqrt(exp(x))

```

----

   3. Currently `nonlinsolve` is not properly capable of solving the system of equations
   having trigonometric functions.

   `solve` can be used for such cases(not all solution):

```python
   >>> solve([sin(x + y), cos(x - y)], [x, y])
   ⎡⎛-3⋅π   3⋅π⎞  ⎛-π   π⎞  ⎛π  3⋅π⎞  ⎛3⋅π  π⎞⎤
   ⎢⎜─────, ───⎟, ⎜───, ─⎟, ⎜─, ───⎟, ⎜───, ─⎟⎥
   ⎣⎝  4     4 ⎠  ⎝ 4   4⎠  ⎝4   4 ⎠  ⎝ 4   4⎠⎦
```

##### In `Julia`:

```jldoctest solvers
julia> solve([sin(x + y), cos(x - y)], [x, y])
4-element Vector{Tuple{Sym, Sym}}:
 (-3*pi/4, 3*pi/4)
 (-pi/4, pi/4)
 (pi/4, 3*pi/4)
 (3*pi/4, pi/4)

```

----


`solveset` reports each solution only once.  To get the solutions of a
polynomial including multiplicity use `roots`.

```python
    >>> solveset(x**3 - 6*x**2 + 9*x, x)
    {0, 3}
    >>> roots(x**3 - 6*x**2 + 9*x, x)
    {0: 1, 3: 2}
```

##### In `Julia`:

```jldoctest solvers
julia> solveset(x^3 - 6*x^2 + 9*x, x)
{0, 3}

```

```jldoctest solvers
julia> roots(x^3 - 6*x^2 + 9*x, x)  |>  d -> convert(Dict{Sym, Any}, d) # prettier priting
Dict{Sym, Any} with 2 entries:
  3 => 2
  0 => 1
```

----

The output `{0: 1, 3: 2}` of `roots` means that `0` is a root of
multiplicity 1 and `3` is a root of multiplicity 2.

> Note:

   Currently `solveset` is not capable of solving the following types of equations:

   * Equations solvable by LambertW (Transcendental equation solver).

   `solve` can be used for such cases:

```python
   >>> solve(x*exp(x) - 1, x )
   [LambertW(1)]
```

##### In `Julia`:

```jldoctest solvers
julia> solve(x*exp(x) - 1, x )
1-element Vector{Sym}:
 W(1)

```

----


## Solving Differential Equations


To solve differential equations, use `dsolve`.  First, create an undefined
function by passing `cls=Function` to the `symbols` function.


```python
    >>> f, g = symbols('f g', cls=Function)
```

##### In `Julia`:

```jldoctest solvers
julia> @syms f() g()
(f, g)

```

----

`f` and `g` are now undefined functions.  We can call `f(x)`, and it
will represent an unknown function.

```python
    >>> f(x)
    f(x)
```

##### In `Julia`:

```jldoctest solvers
julia> f(x)
f(x)

```

----

Derivatives of `f(x)` are unevaluated.

```python
    >>> f(x).diff(x)
    d
    ──(f(x))
    dx
```

##### In `Julia`:

```jldoctest solvers
julia> f(x).diff(x)
d
──(f(x))
dx

```

----

(see the :ref:`Derivatives <tutorial-derivatives>` section for more on
derivatives).

To represent the differential equation $f''(x) - 2f'(x) + f(x) = \sin(x)$, we
would thus use

```python
    >>> diffeq = Eq(f(x).diff(x, x) - 2*f(x).diff(x) + f(x), sin(x))
    >>> diffeq
                          2
             d           d
    f(x) - 2⋅──(f(x)) + ───(f(x)) = sin(x)
             dx           2
                        dx
```

##### In `Julia`:

```jldoctest solvers
julia> diffeq = Eq(f(x).diff(x, x) - 2*f(x).diff(x) + f(x), sin(x)); string(diffeq)
"Eq(f(x) - 2*Derivative(f(x), x) + Derivative(f(x), (x, 2)), sin(x))"

```

----

To solve the ODE, pass it and the function to solve for to `dsolve`.

```python
    >>> dsolve(diffeq, f(x))
                        x   cos(x)
    f(x) = (C₁ + C₂⋅x)⋅ℯ  + ──────
                              2
```

##### In `Julia`:

* we use `dsolve` for initial value proplems

```jldoctest solvers
julia> dsolve(diffeq, f(x)) |> string
"Eq(f(x), (C1 + C2*x)*exp(x) + cos(x)/2)"

```


----

`dsolve` returns an instance of `Eq`.  This is because in general,
solutions to differential equations cannot be solved explicitly for the
function.

```python
    >>> dsolve(f(x).diff(x)*(1 - sin(f(x))), f(x))
    f(x) + cos(f(x)) = C₁
```

##### In `Julia`:

```jldoctest solvers
julia> dsolve(f(x).diff(x)*(1 - sin(f(x))), f(x))
f(x) = C₁

```

----

The arbitrary constants in the solutions from dsolve are symbols of the form
`C1`, `C2`, `C3`, and so on.


## Julia alternative interface




`SymPy.jl` adds a `SymFunction` class, that makes it a bit easier to set up a differential equation, though not as general.

We use either the `SymFunction` constructor

```jldoctest solvers
julia> f = SymFunction("f")
f

```

or the `@syms` macro, as in `@syms f()` to define symbolic functions. The `Differential` function (who's functionality is lifted from `ModelingToolkit`). Can simplify things:

```jldoctest solvers
julia> D = Differential(x);

julia> diffeq = Eq(D(D(f))(x) - 2*D(f)(x) + f(x), sin(x)); string(diffeq)
"Eq(f(x) - 2*Derivative(f(x), x) + Derivative(f(x), (x, 2)), sin(x))"

julia> dsolve(diffeq, f(x))  |> string
"Eq(f(x), (C1 + C2*x)*exp(x) + cos(x)/2)"

```


Or:

```jldoctest solvers
julia> dsolve(D(f)(x)*(1 - sin(f(x))), f(x))
f(x) = C₁

```

Initial conditions can be specified using a dictionary.

For the initial condition `f'(x0) = y0`, this would be specified as `Dict(D(f)(x0) => y0)`.

For example, to solve the exponential equation $f'(x) = f(x), f(0) = a$ we would have:

```jldoctest solvers
julia> @syms x, a, f()
(x, a, f)

julia> dsolve(D(f)(x) - f(x), f(x), ics = Dict(f(0) => a)) |>  string
"Eq(f(x), a*exp(x))"

```

To solve the simple harmonic equation, where two initial conditions are specified, we combine the tuple for each within another tuple:

```jldoctest solvers
julia> ics = Dict(f(0) => 1, D(f)(0) => 2);

julia> dsolve(D(D(f))(x) - f(x), f(x), ics=ics) |> string
"Eq(f(x), 3*exp(x)/2 - exp(-x)/2)"

```
