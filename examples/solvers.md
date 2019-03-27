# Solvers

[From](https://docs.sympy.org/latest/tutorial/solvers.html)


```verbatim
    >>> from sympy import *
    >>> x, y, z = symbols("x y z")
    >>> init_printing(use_unicode=True)
```

##### In `Julia`:

```
using SymPy
x, y, z = symbols("x y z")
```

----

## A Note about Equations


Recall from the :ref:`gotchas <tutorial_gotchas_equals>` section of this
tutorial that symbolic equations in SymPy are not represented by `=` or
`==`, but by `Eq`.


```verbatim
    >>> Eq(x, y)
    x = y
```

##### In `Julia`:

```
Eq(x, y)
```

----

However, there is an even easier way.  In SymPy, any expression not in an
`Eq` is automatically assumed to equal 0 by the solving functions.  Since `a
= b` if and only if `a - b = 0`, this means that instead of using `x == y`,
you can just use `x - y`.  For example

```verbatim
    >>> solveset(Eq(x**2, 1), x)
    {-1, 1}
    >>> solveset(Eq(x**2 - 1, 0), x)
    {-1, 1}
    >>> solveset(x**2 - 1, x)
    {-1, 1}
```

##### In `Julia`:

```
solveset(Eq(x^2, 1), x)
```

```
solveset(Eq(x^2 - 1, 0), x)
```

```
solveset(x^2 - 1, x)
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

```verbatim
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

```
const S = sympy.S
solveset(x^2 - x, x)
```

```
solveset(x - x, x, domain=S.Reals)
```

```
solveset(sin(x) - 1, x, domain=S.Reals)
```

----


If there are no solutions, an `EmptySet` is returned and if it
is not able to find solutions then a `ConditionSet` is returned.

```verbatim
    >>> solveset(exp(x), x)     # No solution exists
    ∅
    >>> solveset(cos(x) - x, x)  # Not able to find solution
    {x | x ∊ ℂ ∧ -x + cos(x) = 0}
```

##### In `Julia`:

```
solveset(exp(x), x)     # No solution exists
```

```
solveset(cos(x) - x, x)  # Not able to find solution
```

----

In the `solveset` module, the linear system of equations is solved using `linsolve`.
In future we would be able to use linsolve directly from `solveset`. Following
is an example of the syntax of `linsolve`.

* List of Equations Form:

```verbatim
    >>> linsolve([x + y + z - 1, x + y + 2*z - 3 ], (x, y, z))
```

##### In `Julia`:

```
linsolve([x + y + z - 1, x + y + 2*z - 3 ], (x, y, z))
```


----

* Augmented
Matrix Form:

```verbatim
	>>> linsolve(Matrix(([1, 1, 1, 1], [1, 1, 2, 3])), (x, y, z))
	{(-y - 1, y, 2)}
```

##### In `Julia`:

```
linsolve(sympy.Matrix(([1, 1, 1, 1], [1, 1, 2, 3])), (x, y, z))
```


----

* A*x = b Form

```verbatim
	>>> M = Matrix(((1, 1, 1, 1), (1, 1, 2, 3)))
	>>> system = A, b = M[:, :-1], M[:, -1]
	>>> linsolve(system, x, y, z)
	{(-y - 1, y, 2)}
```

##### In `Julia`:

```
M = sympy.Matrix(((1, 1, 1, 1), (1, 1, 2, 3)))
system = A, b = M[:, 1:end-1], M[:, end]
linsolve(system, x, y, z)
```


----

!!! note

    The order of solution corresponds the order of given symbols.

In the `solveset` module, the non linear system of equations is solved using
`nonlinsolve`. Following are examples of `nonlinsolve`.

1. When only real solution is present:

```verbatim
	>>> a, b, c, d = symbols('a, b, c, d', real=True)
	>>> nonlinsolve([a**2 + a, a - b], [a, b])
	{(-1, -1), (0, 0)}
	>>> nonlinsolve([x*y - 1, x - 2], x, y)
	{(2, 1/2)}
```

##### In `Julia`:

* we pass `[a,b]` as either `a, b` or using a tuple, as in `(a,b)`, but *not* as a vector, as this gets mapped into a vector of symbolic objects which causes issues with `nonlinsolve`:

```
a, b, c, d = symbols("a, b, c, d", real=true)
nonlinsolve([a^2 + a, a - b], a, b)
```

```
nonlinsolve([x*y - 1, x - 2], x, y)
```

----

2. When only complex solution is present:

```verbatim
	>>> nonlinsolve([x**2 + 1, y**2 + 1], [x, y])
	{(-ⅈ, -ⅈ), (-ⅈ, ⅈ), (ⅈ, -ⅈ), (ⅈ, ⅈ)}
```

##### In `Julia`:

```
nonlinsolve([x^2 + 1, y^2 + 1], (x, y))
```

----

3. When both real and complex solution is present:

```verbatim
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

```
system = [x^2-2*y^2-2, x*y-2]
vars = (x, y)
nonlinsolve(system, vars)
```

However, the next bit requires some modifications to run:

* the `system` array definition must have extra spaces removed
* `Dummy`, `Lambda`, `Mod`, `ImageSet`, `FiniteSet` aren't exported
* we need `PI`, not `pi` to have a symbolic value

```
n = sympy.Dummy("n")
system = [exp(x)-sin(y), 1/y-3]
real_soln = (log(sin(S(1)/3)), S(1)/3)
img_lamda = sympy.Lambda(n, 2*n*IM*PI + sympy.Mod(log(sin(S(1)/3)), 2*IM*PI))
complex_soln = (sympy.ImageSet(img_lamda, S.Integers), S(1)/3)
soln = sympy.FiniteSet(real_soln, complex_soln)
nonlinsolve(system, (x, y)) == soln
```

----

4. If non linear system of equations is Positive dimensional system (A system with
infinitely many solutions is said to be positive-dimensional):

```verbatim
	>>> nonlinsolve([x*y, x*y - x], [x, y])
	{(0, y)}

	>>> system = [a**2 + a*c, a - b]
	>>> nonlinsolve(system, [a, b])
	{(0, 0), (-c, -c)}
```

##### In `Julia`:

* again, we use a tuple for the variables:

```
nonlinsolve([x*y, x*y-x], (x, y))
```

```
system = [a^2+a*c, a-b]
nonlinsolve(system, (a, b))
```


----

> Note:

   1. The order of solution corresponds the order of given symbols.

   2. Currently `nonlinsolve` doesn't return solution in form of `LambertW` (if there
   is solution present in the form of `LambertW`).

   `solve` can be used for such cases:

```verbatim
   >>> solve([x**2 - y**2/exp(x)], [x, y], dict=True)
   ⎡⎧             ⎛y⎞⎫⎤
   ⎢⎨x: 2⋅LambertW⎜─⎟⎬⎥
   ⎣⎩             ⎝2⎠⎭⎦
```

##### In `Julia`:

it is similar

```
u = solve([x^2 - y^2/exp(x)], [x, y], dict=true)
```

To get prettier output, the dict may be converted to have one with symbolic keys:

```
convert(Dict{SymPy.Sym, Any}, first(u))
```

----

   3. Currently `nonlinsolve` is not properly capable of solving the system of equations
   having trigonometric functions.

   `solve` can be used for such cases(not all solution):

```verbatim
   >>> solve([sin(x + y), cos(x - y)], [x, y])
   ⎡⎛-3⋅π   3⋅π⎞  ⎛-π   π⎞  ⎛π  3⋅π⎞  ⎛3⋅π  π⎞⎤
   ⎢⎜─────, ───⎟, ⎜───, ─⎟, ⎜─, ───⎟, ⎜───, ─⎟⎥
   ⎣⎝  4     4 ⎠  ⎝ 4   4⎠  ⎝4   4 ⎠  ⎝ 4   4⎠⎦
```

##### In `Julia`:

```
solve([sin(x + y), cos(x - y)], [x, y])
```

----


`solveset` reports each solution only once.  To get the solutions of a
polynomial including multiplicity use `roots`.

```verbatim
    >>> solveset(x**3 - 6*x**2 + 9*x, x)
    {0, 3}
    >>> roots(x**3 - 6*x**2 + 9*x, x)
    {0: 1, 3: 2}
```

##### In `Julia`:

```
solveset(x^3 - 6*x^2 + 9*x, x)
```

```
roots(x^3 - 6*x^2 + 9*x, x)  |>  d -> convert(Dict{Sym, Any}, d) # prettier priting
```

----

The output `{0: 1, 3: 2}` of `roots` means that `0` is a root of
multiplicity 1 and `3` is a root of multiplicity 2.

> Note:

   Currently `solveset` is not capable of solving the following types of equations:

   * Equations solvable by LambertW (Transcendental equation solver).

   `solve` can be used for such cases:

```verbatim
   >>> solve(x*exp(x) - 1, x )
   [LambertW(1)]
```

##### In `Julia`:

```
solve(x*exp(x) - 1, x )
```

----


## Solving Differential Equations


To solve differential equations, use `dsolve`.  First, create an undefined
function by passing `cls=Function` to the `symbols` function.


```verbatim
    >>> f, g = symbols('f g', cls=Function)
```

##### In `Julia`:

```
f, g = symbols("f g", cls=sympy.Function)
```

----

`f` and `g` are now undefined functions.  We can call `f(x)`, and it
will represent an unknown function.

```verbatim
    >>> f(x)
    f(x)
```

##### In `Julia`:

```
f(x)
```

----

Derivatives of `f(x)` are unevaluated.

```verbatim
    >>> f(x).diff(x)
    d
    ──(f(x))
    dx
```

##### In `Julia`:

```
f(x).diff(x)
```

----

(see the :ref:`Derivatives <tutorial-derivatives>` section for more on
derivatives).

To represent the differential equation $f''(x) - 2f'(x) + f(x) = \sin(x)$, we
would thus use

```verbatim
    >>> diffeq = Eq(f(x).diff(x, x) - 2*f(x).diff(x) + f(x), sin(x))
    >>> diffeq
                          2
             d           d
    f(x) - 2⋅──(f(x)) + ───(f(x)) = sin(x)
             dx           2
                        dx
```

##### In `Julia`:

```
diffeq = Eq(f(x).diff(x, x) - 2*f(x).diff(x) + f(x), sin(x))
diffeq
```

----

To solve the ODE, pass it and the function to solve for to `dsolve`.

```verbatim
    >>> dsolve(diffeq, f(x))
                        x   cos(x)
    f(x) = (C₁ + C₂⋅x)⋅ℯ  + ──────
                              2
```

##### In `Julia`:

* we use `dsolve` for initial value proplems

```
dsolve(diffeq, f(x))
```


----

`dsolve` returns an instance of `Eq`.  This is because in general,
solutions to differential equations cannot be solved explicitly for the
function.

```verbatim
    >>> dsolve(f(x).diff(x)*(1 - sin(f(x))), f(x))
    f(x) + cos(f(x)) = C₁
```

##### In `Julia`:

```
dsolve(f(x).diff(x)*(1 - sin(f(x))), f(x))
```

----

The arbitrary constants in the solutions from dsolve are symbols of the form
`C1`, `C2`, `C3`, and so on.


## Julia alternative interface




`SymPy.jl` adds a `SymFunction` class, that makes it a bit easier to set up a differential equation, though not as general.

We use either the `SymFunction` constructor

```
f = SymFunction("f")
```

or the `@symfuns` macro, as in `@symfuns f`.

to define symbolic functions. For these, rather than use `diff` to specify derivatives, the prime notation can be used. We then have, with `f` defined above:

```
diffeq = Eq(f''(x) - 2*f'(x) + f(x), sin(x))
dsolve(diffeq, f(x))
```

Or:

```
dsolve(f'(x)*(1 - sin(f(x))), f(x))
```

This interface allows a different specification of initial conditions than does `sympy.dsolve`.

For the initial condition `f'(x0) = y0`, this would be specified with a tuple `(f', x0, y0)`.

For example, to solve the exponential equation $f'(x) = f(x), f(0) = a$ we would have:

```
f = SymFunction("f")
x, a = symbols("x, a")
dsolve(f'(x) - f(x), f(x), ics = (f, 0, a))
```

To solve the simple harmonic equation, where two initial conditions are specified, we combine the tuple for each within another tuple:

```
ics = ((f, 0, 1), (f', 0, 2))
dsolve(f''(x) - f(x), f(x), ics=ics)
```


----

[return to index](./index.html)
