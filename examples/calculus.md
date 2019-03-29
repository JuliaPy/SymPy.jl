# Calculus

[From](https://docs.sympy.org/latest/tutorial/calculus.html)

This section covers how to do basic calculus tasks such as derivatives,
integrals, limits, and series expansions in SymPy.  If you are not familiar
with the math of any part of this section, you may safely skip it.

```verbatim
    >>> from sympy import *
    >>> x, y, z = symbols('x y z')
    >>> init_printing(use_unicode=True)
```

##### In `Julia`

we have
* all functions from `sympy` are imported by default
* unicode printing is enabled by default
* double quotes are for strings

so the above can be:

```
using SymPy
x, y, z = symbols("x y z")
```

We can also use the convenient `@vars` macro, as with

```
@vars x y z
```

-----

## Derivatives


To take derivatives, use the `diff` function.

```verbatim
    >>> diff(cos(x), x)
    -sin(x)
    >>> diff(exp(x**2), x)
         ⎛ 2⎞
         ⎝x ⎠
    2⋅x⋅ℯ
```

##### In  `Julia`

save for `**` becoming `^` this is the same

```
diff(cos(x), x)
```

```
diff(exp(x^2), x)
```

----

`diff` can take multiple derivatives at once.  To take multiple derivatives,
pass the variable as many times as you wish to differentiate, or pass a number
after the variable.  For example, both of the following find the third
derivative of `x^4`.

```verbatim
    >>> diff(x**4, x, x, x)
    24⋅x
    >>> diff(x**4, x, 3)
    24⋅x
```

##### In `Julia`

```
diff(x^4, x, x, x)
```

```
diff(x^4, x, 3)
```

----

You can also take derivatives with respect to many variables at once.  Just
pass each derivative in order, using the same syntax as for single variable
derivatives.  For example, each of the following will compute
$\frac{\partial^7}{\partial x\partial y^2\partial z^4} e^{x y z}$.

```verbatim
    >>> expr = exp(x*y*z)
    >>> diff(expr, x, y, y, z, z, z, z)
     3  2 ⎛ 3  3  3       2  2  2                ⎞  x⋅y⋅z
    x ⋅y ⋅⎝x ⋅y ⋅z  + 14⋅x ⋅y ⋅z  + 52⋅x⋅y⋅z + 48⎠⋅ℯ
    >>> diff(expr, x, y, 2, z, 4)
     3  2 ⎛ 3  3  3       2  2  2                ⎞  x⋅y⋅z
    x ⋅y ⋅⎝x ⋅y ⋅z  + 14⋅x ⋅y ⋅z  + 52⋅x⋅y⋅z + 48⎠⋅ℯ
    >>> diff(expr, x, y, y, z, 4)
     3  2 ⎛ 3  3  3       2  2  2                ⎞  x⋅y⋅z
    x ⋅y ⋅⎝x ⋅y ⋅z  + 14⋅x ⋅y ⋅z  + 52⋅x⋅y⋅z + 48⎠⋅ℯ
```

##### In `Julia`:

```
expr = exp(x*y*z)
```

```
diff(expr, x, y, y, z, z, z, z)
```

```
diff(expr, x, y, 2, z, 4)
```

```
diff(expr, x, y, y, z, 4)
```

----

`diff` can also be called as a method.  The two ways of calling `diff` are exactly the same, and are provided only for convenience.

```verbatim
    >>> expr.diff(x, y, y, z, 4)
     3  2 ⎛ 3  3  3       2  2  2                ⎞  x⋅y⋅z
    x ⋅y ⋅⎝x ⋅y ⋅z  + 14⋅x ⋅y ⋅z  + 52⋅x⋅y⋅z + 48⎠⋅ℯ
```

##### In `Julia`:

```
expr.diff(x, y, y, z, 4)
```

----

To create an unevaluated derivative, use the `Derivative` class.  It has the same syntax as `diff`.

```verbatim
    >>> deriv = Derivative(expr, x, y, y, z, 4)
    >>> deriv
         7
        ∂     ⎛ x⋅y⋅z⎞
    ──────────⎝ℯ     ⎠
      4   2
      ∂z  ∂y  ∂x
```

##### In `Julia`,

classes are not exported, so we use `sympy.Derivative`:

```
deriv = sympy.Derivative(expr, x, y, y, z, 4)
deriv
```

----

To evaluate an unevaluated derivative, use the `doit` method.

```verbatim
    >>> deriv.doit()
     3  2 ⎛ 3  3  3       2  2  2                ⎞  x⋅y⋅z
    x ⋅y ⋅⎝x ⋅y ⋅z  + 14⋅x ⋅y ⋅z  + 52⋅x⋅y⋅z + 48⎠⋅ℯ
```

##### In `Julia`:

```
deriv.doit()
```

----

These unevaluated objects are useful for delaying the evaluation of the
derivative, or for printing purposes.  They are also used when SymPy does not
know how to compute the derivative of an expression (for example, if it
contains an undefined function, which are described in the :ref:`Solving
Differential Equations <tutorial-dsolve>` section).

Derivatives of unspecified order can be created using tuple `(x, n)` where
`n` is the order of the derivative with respect to `x`.

```verbatim
    >>> m, n, a, b = symbols('m n a b')
    >>> expr = (a*x + b)**m
    >>> expr.diff((x, n))
      n
     ∂ ⎛         m⎞
    ───⎝(a⋅x + b) ⎠
      n
    ∂x
```

##### In `Julia`:

```
@vars m n a b
expr = (a*x + b)^m
expr.diff((x, n))
```

----

## Integrals


To compute an integral, use the `integrate` function.  There are two kinds
of integrals, definite and indefinite.  To compute an indefinite integral,
that is, an antiderivative, or primitive, just pass the variable after the
expression.

```verbatim
    >>> integrate(cos(x), x)
    sin(x)
```

##### In `Julia`:

```
integrate(cos(x), x)
```

----

Note that SymPy does not include the constant of integration.  If you want it,
you can add one yourself, or rephrase your problem as a differential equation
and use `dsolve` to solve it, which does add the constant (see :ref:`tutorial-dsolve`).

> Quick Tip:

   $\infty$ in SymPy is `oo` (that's the lowercase letter "oh" twice).  This
   is because `oo` looks like $\infty$, and is easy to type.

----

To compute a definite integral, pass the argument `(integration_variable,
lower_limit, upper_limit)`.  For example, to compute

$$~

   \int_0^\infty e^{-x}\,dx,
~$$
we would do

```verbatim
    >>> integrate(exp(-x), (x, 0, oo))
    1
```

##### In Julia:

```
integrate(exp(-x), (x, 0, oo))
```

----

As with indefinite integrals, you can pass multiple limit tuples to perform a
multiple integral.  For example, to compute


$$~
   \int_{-\infty}^{\infty}\int_{-\infty}^{\infty} e^{- x^{2} - y^{2}}\, dx\, dy,
~$$
do

```verbatim
    >>> integrate(exp(-x**2 - y**2), (x, -oo, oo), (y, -oo, oo))
    π
```

##### In `Julia`:

```
integrate(exp(-x^2 - y^2), (x, -oo, oo), (y, -oo, oo))
```

----

If `integrate` is unable to compute an integral, it returns an unevaluated
`Integral` object.

```verbatim
    >>> expr = integrate(x**x, x)
    >>> print(expr)
    Integral(x**x, x)
    >>> expr
    ⌠
    ⎮  x
    ⎮ x  dx
    ⌡
```


##### In Julia:

```
expr = integrate(x^x, x)
expr
```

----

As with `Derivative`, you can create an unevaluated integral using
`Integral`.  To later evaluate this integral, call `doit`.

```verbatim
    >>> expr = Integral(log(x)**2, x)
    >>> expr
    ⌠
    ⎮    2
    ⎮ log (x) dx
    ⌡
    >>> expr.doit()
             2
    x⋅log (x) - 2⋅x⋅log(x) + 2⋅x
```

##### In `Julia`:

the `Integral` class is not exported, so it must be qualified:

```
expr = sympy.Integral(log(x)^2, x)
expr
```

```
expr.doit()
```

----

`integrate` uses powerful algorithms that are always improving to compute
both definite and indefinite integrals, including heuristic pattern matching
type algorithms, a partial implementation of the `Risch algorithm
<http://en.wikipedia.org/wiki/Risch_algorithm>`, and an algorithm using
`Meijer G-functions <http://en.wikipedia.org/wiki/Meijer_g-function>` that is
useful for computing integrals in terms of special functions, especially
definite integrals.  Here is a sampling of some of the power of `integrate`.

```verbatim
    >>> integ = Integral((x**4 + x**2*exp(x) - x**2 - 2*x*exp(x) - 2*x -
    ...     exp(x))*exp(x)/((x - 1)**2*(x + 1)**2*(exp(x) + 1)), x)
    >>> integ
    ⌠
    ⎮ ⎛ 4    2  x    2        x          x⎞  x
    ⎮ ⎝x  + x ⋅ℯ  - x  - 2⋅x⋅ℯ  - 2⋅x - ℯ ⎠⋅ℯ
    ⎮ ──────────────────────────────────────── dx
    ⎮               2        2 ⎛ x    ⎞
    ⎮        (x - 1) ⋅(x + 1) ⋅⎝ℯ  + 1⎠
    ⌡
    >>> integ.doit()
                     x
       ⎛ x    ⎞     ℯ
    log⎝ℯ  + 1⎠ + ──────
                   2
                  x  - 1

    >>> integ = Integral(sin(x**2), x)
    >>> integ
    ⌠
    ⎮    ⎛ 2⎞
    ⎮ sin⎝x ⎠ dx
    ⌡
    >>> integ.doit()
                    ⎛√2⋅x⎞
    3⋅√2⋅√π⋅fresnels⎜────⎟⋅Γ(3/4)
                    ⎝ √π ⎠
    ─────────────────────────────
               8⋅Γ(7/4)

    >>> integ = Integral(x**y*exp(-x), (x, 0, oo))
    >>> integ
    ∞
    ⌠
    ⎮  y  -x
    ⎮ x ⋅ℯ   dx
    ⌡
    0
    >>> integ.doit()
    ⎧ Γ(y + 1)    for -re(y) < 1
    ⎪
    ⎪∞
    ⎪⌠
    ⎨⎮  y  -x
    ⎪⎮ x ⋅ℯ   dx    otherwise
    ⎪⌡
    ⎪0
    ⎩
```

##### In `Julia`:

```
integ = sympy.Integral((x^4 + x^2*exp(x) - x^2 - 2*x*exp(x) - 2*x - exp(x))*exp(x)/((x - 1)^2*(x + 1)^2*(exp(x) + 1)), x)
integ
```

```
integ.doit()
```

```
integ = sympy.Integral(sin(x^2), x)
integ.doit()
```

```
integ = sympy.Integral(x^y*exp(-x), (x, 0, oo))
integ.doit()
```

----

This last example returned a `Piecewise` expression because the integral
does not converge unless $\Re(y) > 1.$

## Limits


SymPy can compute symbolic limits with the `limit` function.  The syntax to compute

$$~
   \lim_{x\to x_0} f(x)
~$$

is `limit(f(x), x, x0)`.

```verbatim
    >>> limit(sin(x)/x, x, 0)
    1
```

##### In `Julia`:

```
limit(sin(x)/x, x, 0)
```

In `Julia`, a pair can be used to indicate the limit:

```
limit(sin(x)/x, x=>0)
```

----

`limit` should be used instead of `subs` whenever the point of evaluation
is a singularity.  Even though SymPy has objects to represent $\infty$, using
them for evaluation is not reliable because they do not keep track of things
like rate of growth.  Also, things like $\infty - \infty$ and
$\frac{\infty}{\infty}$ return $\mathrm{nan}$ (not-a-number).  For example

```verbatim
    >>> expr = x**2/exp(x)
    >>> expr.subs(x, oo)
    nan
    >>> limit(expr, x, oo)
    0
```

##### In `Julia`:

```
expr = x^2/exp(x)
expr.subs(x, oo)
```

```
limit(expr, x, oo)
```

----

Like `Derivative` and `Integral`, `limit` has an unevaluated
counterpart, `Limit`.  To evaluate it, use `doit`.

```verbatim
    >>> expr = Limit((cos(x) - 1)/x, x, 0)
    >>> expr
         ⎛cos(x) - 1⎞
     lim ⎜──────────⎟
    x─→0⁺⎝    x     ⎠
    >>> expr.doit()
    0
```

##### In `Julia`:

```
expr = sympy.Limit((cos(x) - 1)/x, x, 0)
expr
```

```
expr.doit()
```

To evaluate a limit at one side only, pass `'+'` or `'-'` as a third
argument to `limit`.  For example, to compute

$$~
   \lim_{x\to 0^+}\frac{1}{x},
~$$

do

```verbatim
    >>> limit(1/x, x, 0, '+')
    ∞
```

##### In `Julia`:

```
limit(1/x, x => 0, "+")
```

----

As opposed to

```verbatim
    >>> limit(1/x, x, 0, '-')
    -∞
```


##### In `Julia`:

```
limit(1/x, x, 0, "-")
```

----


## Series Expansion


SymPy can compute asymptotic series expansions of functions around a point. To
compute the expansion of `f(x)` around the point `x = x_0` terms of order
`x^n`, use `f(x).series(x, x0, n)`.  `x0` and `n` can be omitted, in
which case the defaults `x0=0` and `n=6` will be used.

```verbatim
    >>> expr = exp(sin(x))
    >>> expr.series(x, 0, 4)
             2
            x     ⎛ 4⎞
    1 + x + ── + O⎝x ⎠
            2
```

##### In `Julia`:

```
expr = exp(sin(x))
expr.series(x, 0, 4)
```

----

The `O\left (x^4\right )` term at the end represents the Landau order term at
`x=0` (not to be confused with big O notation used in computer science, which
generally represents the Landau order term at $x=\infty$).  It means that all
x terms with power greater than or equal to `x^4` are omitted.  Order terms
can be created and manipulated outside of `series`.  They automatically
absorb higher order terms.

```verbatim
    >>> x + x**3 + x**6 + O(x**4)
         3    ⎛ 4⎞
    x + x  + O⎝x ⎠
    >>> x*O(1)
    O(x)
```

##### In `Julia`:

`O` is not exported, so we must qualify it:


```
x + x^3 + x^6 + sympy.O(x^4)

```

```
x*sympy.O(1)
```

----

If you do not want the order term, use the `removeO` method.

```verbatim
    >>> expr.series(x, 0, 4).removeO()
     2
    x
    ── + x + 1
    2
```

##### In `Julia`:

```
expr.series(x, 0, 4).removeO()
```

----

The `O` notation supports arbitrary limit points (other than 0):

```verbatim
    >>> exp(x - 6).series(x, x0=6)
                2          3          4          5
         (x - 6)    (x - 6)    (x - 6)    (x - 6)         ⎛       6       ⎞
    -5 + ──────── + ──────── + ──────── + ──────── + x + O⎝(x - 6) ; x → 6⎠
            2          6          24        120
```

##### In `Julia`:

```
exp(x - 6).series(x, x0=6)
```

## Finite differences


So far we have looked at expressions with analytic derivatives
and primitive functions respectively. But what if we want to have an
expression to estimate a derivative of a curve for which we lack a
closed form representation, or for which we don't know the functional
values for yet. One approach would be to use a finite difference
approach.

The simplest way the differentiate using finite differences is to use
the `differentiate_finite` function:

```verbatim
    >>> f, g = symbols('f g', cls=Function)
    >>> differentiate_finite(f(x)*g(x))
    -f(x - 1/2)⋅g(x - 1/2) + f(x + 1/2)⋅g(x + 1/2)
```

##### In `Julia`:

* `differentiate_finite` is not exported

```
f, g = symbols("f g", cls=sympy.Function)
sympy.differentiate_finite(f(x)*g(x))
```

(The functions `f` and `g` can also be created with the command `@symfuns f g`, using the `@symfuns` macro.)

----

If we want to expand the intermediate derivative we may pass the
flag `evaluate=True`:

```verbatim
    >>> differentiate_finite(f(x)*g(x), evaluate=True)
    (-f(x - 1/2) + f(x + 1/2))⋅g(x) + (-g(x - 1/2) + g(x + 1/2))⋅f(x)
```

##### In `Julia`:

```
sympy.differentiate_finite(f(x)*g(x), evaluate=true)
```

----

This form however does not respect the product rule.

If you already have a `Derivative` instance, you can use the
`as_finite_difference` method to generate approximations of the
derivative to arbitrary order:

```verbatim
    >>> f = Function('f')
    >>> dfdx = f(x).diff(x)
    >>> dfdx.as_finite_difference()
    -f(x - 1/2) + f(x + 1/2)
```

##### In `Julia`:

```
f = sympy.Function("f")
dfdx = f(x).diff(x)
dfdx.as_finite_difference()
```

----

here the first order derivative was approximated around x using a
minimum number of points (2 for 1st order derivative) evaluated
equidistantly using a step-size of 1. We can use arbitrary steps
(possibly containing symbolic expressions):

```verbatim
    >>> f = Function('f')
    >>> d2fdx2 = f(x).diff(x, 2)
    >>> h = Symbol('h')
    >>> d2fdx2.as_finite_difference([-3*h,-h,2*h])
    f(-3⋅h)   f(-h)   2⋅f(2⋅h)
    ─────── - ───── + ────────
         2        2        2
      5⋅h      3⋅h     15⋅h
```

##### In `Julia`:

```
f = sympy.Function("f")
d2fdx2 = f(x).diff(x, 2)
h = sympy.Symbol("h")
d2fdx2.as_finite_difference([-3*h,-h,2*h])
```

----

If you are just interested in evaluating the weights, you can do so
manually:

```verbatim
    >>> finite_diff_weights(2, [-3, -1, 2], 0)[-1][-1]
    [1/5, -1/3, 2/15]
```

##### In `Julia`:

the `finite_diff_weights` function that is not exported:


```
sympy.finite_diff_weights(2, [-3, -1, 2], 0)[end][end]
```

----

note that we only need the last element in the last sublist
returned from `finite_diff_weights`. The reason for this is that
the function also generates weights for lower derivatives and
using fewer points (see the documentation of `finite_diff_weights`
for more details).

If using `finite_diff_weights` directly looks complicated, and the
`as_finite_difference` method of `Derivative` instances
is not flexible enough, you can use `apply_finite_diff` which
takes `order`, `x_list`, `y_list` and `x0` as parameters:

```verbatim
    >>> x_list = [-3, 1, 2]
    >>> y_list = symbols('a b c')
    >>> apply_finite_diff(1, x_list, y_list, 0)
      3⋅a   b   2⋅c
    - ─── - ─ + ───
       20   4    5
```

##### In `Julia`,

* `apply_finite_diff` is not exported:

```
x_list = [-3, 1, 2]
y_list = symbols("a b c")
sympy.apply_finite_diff(1, x_list, y_list, 0)
```

----

[return to index](./index.html)
