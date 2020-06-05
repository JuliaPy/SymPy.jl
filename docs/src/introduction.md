# A SymPy introduction

This document provides an introduction to using `SymPy` within `Julia`.
It owes an enormous debt to the tutorial for using SymPy within Python which may be found
[here](http://docs.sympy.org/dev/tutorial/index.html). The overall structure and many examples are taken from that work, with adjustments and additions to illustrate the differences due to using `SymPy` within `Julia`.


After installing `SymPy`, which is discussed in the package's `README`
file, we must first load it into `Julia` with the standard command
`using`:


```
using SymPy
```

The start up time is a bit lengthy.

## Symbols

At the core of `SymPy` is the introduction of symbolic variables that
differ quite a bit from `Julia`'s variables. Symbolic variables do not
immediately evaluate to a value, rather the "symbolicness" propagates
when interacted with. To keep things manageable, SymPy does some
simplifications along the way.

Symbolic expressions are primarily of the `Sym` type and can be constructed in the standard way:

```
x = Sym("x")
```

This creates a symbolic object `x`, which can be manipulated through further function calls.


There is the `@vars` macro that makes creating multiple variables a
bit less typing, as it creates variables in the local scope -- no
assignment is necessary. Compare these similar ways to create symbolic
variables:

```
@vars a b c
a,b,c = Sym("a,b,c")
```

(There is the identical `@syms` for MATLAB users.)

### Assumptions

Finally, there is the `symbols` constructor for producing symbolic objects. With `symbols` it is
possible to pass assumptions onto the variables. A list of possible
assumptions is
[here](http://docs.sympy.org/dev/modules/core.html#module-sympy.core.assumptions). Some
examples are:

```
u = symbols("u")
x = symbols("x", real=true)
y1, y2 = symbols("y1, y2", positive=true)
alpha = symbols("alpha", integer=true, positive=true)
```

As seen, the `symbols` function can be used to make one or more variables with zero, one or more assumptions.

We jump ahead for a second to illustrate, but here we see that `solve` will respect these assumptions, by failing to find solutions to these equations:

```
solve(x^2 + 1)   # ±i are not real
```

```
solve(y1 + 1)    # -1 is not positive
```

The `@vars` macro can also have assumptions passed in as follows:

```
@vars u1 u2 positive=true
solve(u1 + u2)  # empty, though solving u1 - u2 is not.
```


As can be seen, there are several ways to create symbolic values. One
caveat is that one can't use `Sym` to create a variable from a
function name in Base.

### Special constants

`Julia` has its math constants, like `pi` and `e`, `SymPy` as well. A few of these have `Julia` counterparts provided by `SymPy`. For example, these two constants are defined (where `oo` is for infinity):

```
PI,  oo
```

(The pretty printing of SymPy objects does not work for tuples.)

Numeric values themselves can be symbolic. This example shows the
difference. The first `asin` call dispatches to `Julia`'s `asin`
function, the second to `SymPy`'s:

```
[asin(1), asin(Sym(1))]
```

## Substitution

SymPy provides a means to substitute values in for the symbolic expressions. The specification requires an expression, a variable in the expression to substitute in for, and a new value. For example, this is one way to make a polynomial in a new variable:

```
@vars x y
ex = x^2 + 2x + 1
ex.subs(x, y)
```


Substitution can also be numeric:

```
ex.subs(x, 0)
```

The output has no free variables, but is still symbolic.

Expressions with more than one variable can have multiple substitutions, where each is expressed as a tuple:

```
x,y,z = symbols("x,y,z")
ex = x + y + z
ex.subs((x,1), (y,pi))
```

!!! note

    The calling pattern for `subs` is different from a typical `Julia` function call. The `subs` call is `object.method(arguments)` whereas a more "`Julia`n" function call is `method(objects, other objects....)`, as `Julia` offers multiple dispatch of methods. `SymPy` uses the Python calling method, adding in `Julia`n style when appropriate for generic usage within `Julia`. In addition, `SymPy` imports all functions from the underlying `sympy` module and specializes them on a symbolic first argument.

    For `subs`, the simple substitution `ex.object(x,a)` is similar to simple function evaluation, so `Julia`'s call notation will work. To specify the pairing off of `x` and `a`, the `=>`  pairs notation is used.

This calling style will be equivalent to the last:

```
ex(x=>1, y=>pi)
```

A straight call is also possble, where the order of the variables is determined by `free_symbols`:

```
ex(1, pi)
```

This is useful for expressions of a single variable, but being more explicit through the use of paired values would be recommended.

## Conversion from symbolic to numeric

SymPy provides two identical means to convert a symbolic math
expression to a number. One is `evalf`, the other `N`. Within `Julia`
we decouple this, using `N` to also convert to a `Julian` value and
`evalf` to leave the conversion as a symbolic object.  The `N`
function converts symbolic integers, rationals, irrationals, and
complex values, while attempting to find an appropriate `Julia` type
for the value.

To see the difference, we use both on `PI`:

```
N(PI)  # converts to underlying pi irrational
```

Whereas, `evalf` will produce a symbolic numeric value:

```
(PI).evalf()
```


The `evalf` call allows for a precision argument to be passed through the second argument. This is how 30 digits of $\pi$ can be extracted:

```
PI.evalf(30)
```

This is a SymPy, symbolic number, not a `Julia` object. Composing with `N`

```
N(PI.evalf(30))
```

will produce a `Julia` number,


Explicit conversion via `convert(T, ex)` can also be done, and is
necessary at times if `N` does not give the desired type.

## Algebraic expressions

`SymPy` overloads many of `Julia`'s functions to work with symbolic objects, such as seen above with `asin`. The usual mathematical operations such as `+`, `*`, `-`, `/` etc. work through `Julia`'s promotion mechanism, where numbers are promoted to symbolic objects, others dispatch internally to related `SymPy` functions.

In most all  cases, thinking about this distinction between numbers and symbolic numbers is unnecessary, as numeric values passed to `SymPy` functions are typically promoted to symbolic expressions. This conversion will take math constants to their corresponding `SymPy` counterpart, rational expressions to rational expressions, and floating point values to floating point values. However there are edge cases. An expression like `1//2 * pi * x` will differ from the seemingly identical  `1//2 * (pi * x)`. The former will produce a floating point value from `1//2 * pi` before being promoted to a symbolic instance. Using the symbolic value `PI` makes this expression work either way.

Most of `Julia`'s
[mathematical](http://julia.readthedocs.org/en/latest/manual/mathematical-operations/#elementary-functions)
functions are overloaded to work with symbolic expressions. `Julia`'s
generic definitions are used, as possible. This also introduces some
edge cases. For example, `x^(-2)` will balk due to the negative,
integer exponent, but either `x^(-2//1)` or `x^Sym(-2)` will work as
expected, as the former call first dispatches to a generic defintion,
but the latter two expressions do not.


`SymPy` makes it very easy to work with polynomial and rational expressions. First we create some variables:

```
@vars x y z
```

### The expand, factor, collect, and simplify functions

A typical polynomial expression in a single variable can be written in two common ways, expanded or factored form. Using `factor` and `expand` can move between the two.

For example,

```
p = x^2 + 3x + 2
factor(p)
```

Or

```
expand(prod((x-i) for i in 1:5))
```

The `factor` function factors over the rational numbers, so something like this with obvious factors is not finished:

```
factor(x^2 - 2)
```

When expressions involve one or more variables, it can be convenient to be able to manipulate them. For example,
if we define `q` by:

```
q = x*y + x*y^2 + x^2*y + x
```

Then we can collect the terms by the variable `x`:

```
collect(q, x)
```

or the variable `y`:

```
collect(q, y)
```

These are identical expressions, though viewed differently.

A more broad-brush approach is to let `SymPy` simplify the values. In this case, the common value of `x` is factored out:

```
simplify(q)
```

The `simplify` function attempts to apply the dozens of functions related to simplification that are part of SymPy. It is also possible to apply these functions one at a time, for example `trigsimp` does trigonometric simplifications.

The SymPy tutorial illustrates that `expand` can also result in simplifications through this example:

```
expand((x + 1)*(x - 2) - (x - 1)*x)
```


These methods are not restricted to polynomial expressions and will
work with other expressions. For example, `factor` identifies the
following as a factorable object in terms of the variable `exp(x)`:

```
factor(exp(2x) + 3exp(x) + 2)
```

## Rational expressions: apart, together, cancel

When working with rational expressions, SymPy does not do much
simplification unless asked. For example this expression is not
simplified:

```
r = 1/x + 1/x^2
```

To put the terms of `r` over a common denominator, the `together` function is available:

```
together(r)
```

The `apart` function does the reverse, creating a partial fraction decomposition from a ratio of polynomials:

```
apart( (4x^3 + 21x^2 + 10x + 12) /  (x^4 + 5x^3 + 5x^2 + 4x))
```

Some times SymPy will cancel factors, as here:

```
top = (x-1)*(x-2)*(x-3)
bottom = (x-1)*(x-4)
top/bottom
```

(This might make math faculty a bit upset, but it is in line with student thinking.)

However, with expanded terms, the common factor of `(x-1)` is not cancelled:

```
r = expand(top) / expand(bottom)
```

The `cancel` function instructs SymPy to perform cancellations. It
takes rational functions and puts them in a canonical $p/q$ form with
no common (rational) factors and leading terms which are integers:

```
cancel(r)
```

## Powers

The SymPy [tutorial](http://docs.sympy.org/dev/tutorial/simplification.html#powers) offers a thorough explanation on powers and which get simplified and under what conditions. Basically

* $x^a x^b = x^{a+b}$ is always true. However

* $x^a y^a=(xy)^a$ is only true with assumptions, such as $x,y \geq 0$ and $a$ is real, but not in general. For example, $x=y=-1$ and $a=1/2$ has $x^a \cdot y^a = i \cdot i =  -1$, where as $(xy)^a = 1$.

* $(x^a)^b = x^{ab}$ is only true with assumptions. For example $x=-1, a=2$, and $b=1/2$ gives $(x^a)^b = 1^{1/2} = 1$, whereas $x^{ab} = -1^1 = -1$.


We see that with assumptions, the following expression does simplify to $0$:

```
@vars x y nonnegative=true a real=true
simplify(x^a * y^a - (x*y)^a)
```

However, without assumptions this is not the case

```
x,y,a = symbols("x,y,a")
simplify(x^a * y^a - (x*y)^a)
```

The `simplify` function calls `powsimp` to simplify powers, as above. The `powsimp` function has the keyword argument `force=true` to force simplification even if assumptions are not specified:

```
powsimp(x^a * y^a - (x*y)^a, force=true)
```

## Trigonometric simplification

For trigonometric expressions, `simplify` will use `trigsimp` to simplify:

```
theta = symbols("theta", real=true)
p = cos(theta)^2 + sin(theta)^2
```

Calling either `simplify` or `trigsimp` will apply the Pythagorean identity:

```
simplify(p)
```

While often forgotten,  the `trigsimp` function is, of course,  aware of the double angle formulas:

```
simplify(sin(2theta) - 2sin(theta)*cos(theta))
```

The `expand_trig` function will expand such expressions:

```
expand_trig(sin(2theta))
```


## Coefficients

Returning to polynomials, there are a few functions to find various pieces of the polynomials. First we make a general quadratic polynomial:

```
a,b,c,x = symbols("a, b, c, x")
p = a*x^2 + b*x + c
```

If given a polynomial, like `p`, there are different means to extract the coefficients:

* SymPy provides a `coeffs` method for `Poly` objects, but `p` must first be converted to one.

* SymPy provides the `coeff` method for expressions, which allows extration of a coeffiecient for a given monomial




The `ex.coeff(monom)` call will return the corresponding coefficient of the monomial:

```
p.coeff(x^2) # a
p.coeff(x)   # b
```

The constant can be found through substitution:

```
p(x=>0)
```

Though one could use some trick like this to find all the coefficients:

```
Sym[[p.coeff(x^i) for i in N(degree(p,gen=x)):-1:1]; p(x=>0)]
```

that is cumbersome, at best. SymPy has a function `coeffs`, but it is defined for polynomial types, so will fail on `p`:


```
p.coeffs() # fails
```

Polynomials are a special class in SymPy and must be constructed. The `Poly` constructor can be used. As there is more than one free variable in `p`, we specify the variable `x` below:

```
q = sympy.Poly(p, x)
q.coeffs()
```

!!! note

    The `Poly` constructor from SymPy is *not* a function, so is not exported when `SymPy` is loaded. To access it, the object must be qualified by its containing module, in this case `Poly`. Were it to be used frequently, an alias could be used, as in `const Poly=sympy.Poly` *or* the `import_from` function, as in `import_from(sympy, :Poly)`. The latter has some attempt to avoid naming collisions.


## Polynomial roots: solve, real_roots, polyroots, nroots

SymPy provides functions to find the roots of a polynomial. In
general, a polynomial with real coefficients of degree $n$ will have
$n$ roots when multiplicities and complex roots are accounted for. The
number of real roots is consequently between $0$ and $n$.

For a *univariate* polynomial expression (a single variable), the real
roots, when available, are returned by `roots`. For example,

```
real_roots(x^2 - 2)
```

Unlike `factor` -- which only factors over rational factors --
`real_roots` finds the two irrational roots here. It is well known
(the
[Abel-Ruffini theorem](http://en.wikipedia.org/wiki/Abel%E2%80%93Ruffini_theorem))
that for degree 5 polynomials, or higher, it is not always possible to
express the roots in terms of radicals. However, when the roots are
rational `SymPy` can have success:


```
p = (x-3)^2*(x-2)*(x-1)*x*(x+1)*(x^2 + x + 1)
real_roots(p)
```

In this example, the degree of `p` is 8, but only the 6 real roots
returned, the double root of $3$ is accounted for. The two complex
roots of `x^2 + x+ 1` are not considered by this function. The complete set
of distinct roots can be found with `solve`:

```
solve(p)
```

This finds the complex roots, but does not account for the double
root. The `roots` function of SymPy does.



The output of calling `roots` will be a dictionary whose keys are the roots and values the multiplicity.

```
roots(p)
```

When exact answers are not provided, the `roots` call is contentless:

```
p = x^5 - x + 1
roots(p)
```

Calling `solve` seems to produce very little as well:

```
rts = solve(p)
```

But in fact, `rts` contains lots of information. We can extract numeric values quite easily with `N`:

```
N.(rts)
```

These are numeric approximations to irrational values. For numeric
approximations to polynomial roots, the `nroots` function is also
provided, though with this call the answers are still symbolic:

```
nroots(p)
```

## The solve function

The `solve` function is more general purpose than just finding roots of univariate polynomials. The function tries to solve for when an expression is 0, or a set of expressions are all 0.

For example, it can be used to solve when $\cos(x) = \sin(x)$:

```
solve(cos(x) - sin(x))
```

Though there are infinitely many correct solutions, these are within a certain range.

The
[solveset](http://docs.sympy.org/latest/modules/solvers/solveset.html)
function appears in version 1.0 of SymPy and is an intended
replacement for `solve`. Here we see it gives all solutions:

```
u = solveset(cos(x) - sin(x))
```

The output of `solveset` is a set, rather than a vector or
dictionary. To get the values requires some work. For *finite sets* we collect the elements
with `collect`, but first we must convert to a `Julia` `Set`:

```
v = solveset(x^2 - 4)
collect(Set(v...))
```

This composition is done in the `elements` function:

```
elements(v)
```

The `elements` function does not work for more complicated (non-finite) sets, such as `u`. For these, the `contains` method may be useful to query the underlying elements



Solving within Sympy has limits. For example, there is no symbolic solution here:

```
solve(cos(x) - x)
```

(And hence the error message generated.)

For such an equation, a numeric method would be needed, similar to the `Roots` package. For example:

```
nsolve(cos(x) - x, 1)
```

Though it can't solve everything, the `solve` function can also solve
equations of a more general type. For example, here it is used to
derive the quadratic equation:

```
a,b,c  = symbols("a,b,c", real=true)
p = a*x^2 + b*x + c
solve(p, x)
```

The extra argument `x` is passed to `solve` so that `solve` knows
which variable to solve for.

The `solveset` function is similar:

```
solveset(p, x)
```


If the `x` value is not given, `solveset` will complain and  `solve` tries to find a
solution with all the free variables:

```
solve(p)
```

Systems of equations can be solved as well. We specify them within a
vector of expressions, `[ex1, ex2, ..., exn]` where a found solution
is one where all the expressions are 0. For example, to solve this
linear system: $2x + 3y = 6, 3x - 4y=12$, we have:

```
x, y = symbols("x,y", real=true)
exs = [2x+3y-6, 3x-4y-12]
d = solve(exs)
```

We can "check our work" by plugging into each equation. We take advantage of how the `subs` function allows us to pass in a dictionary:

```
map(ex -> ex.subs(d), exs)
```

In the previous example, the system had two equations and two
unknowns. When that is not the case, one can specify the variables to
solve for as a vector. In this example, we find a quadratic polynomial
that approximates $\cos(x)$ near $0$:

```
a,b,c,h = symbols("a,b,c,h", real=true)
p = a*x^2 + b*x + c
fn = cos
exs = [fn(0*h)-p(x=>0), fn(h)-p(x => h), fn(2h)-p(x => 2h)]
d = solve(exs, [a,b,c])
```

Again, a dictionary is returned. The polynomial itself can be found by
substituting back in for `a`, `b`, and `c`:

```
quad_approx = p.subs(d)
```

(Taking the limit as $h$ goes to 0 produces the answer $1 - x^2/2$.)

Finally for `solve`, we show one way to re-express the polynomial $a_2x^2 + a_1x + a_0$
as $b_2(x-c)^2 + b_1(x-c) + b_0$ using `solve` (and not, say, an
expansion theorem.)

```
n = 3
x, c = symbols("x,c")
as = Sym["a$i" for i in 0:(n-1)]
bs = Sym["b$i" for i in 0:(n-1)]
p = sum([as[i+1]*x^i for i in 0:(n-1)])
q = sum([bs[i+1]*(x-c)^i for i in 0:(n-1)])
solve(p-q, bs)
```


### Solving using logical operators

The `solve` function does not need to just solve `ex = 0`. There are other means to specify an equation. Ideally, it would be nice to say `ex1 == ex2`, but the interpretation of `==` is not for this. Rather, `SymPy` introduces `Eq` for equality. So this expression

```
solve(Eq(x, 1))
```

gives 1, as expected from solving `x == 1`.

In addition to `Eq`, there are `Lt`, `Le`, `Ge`, `Gt`. The Unicode
operators are not aliased to these, but there are alternatives
`\ll[tab]`, `\leqq[tab]`, `\Equal[tab]`, `\geqq[tab]`, `\gg[tab]` and
`\neg[tab]` to negate.

So, the above could have been written with the following nearly identical expression, though it is entered with `\Equal[tab]`.

```
solve(x ⩵ 1)
```

Here is an alternative way of asking a previous question on a pair of linear equations:

```
x, y = symbols("x,y", real=true)
exs = [2x+3y ⩵ 6, 3x-4y ⩵ 12]    ## Using \Equal[tab]
d = solve(exs)
```

## Plotting

The `Plots` package allows many 2-dimensional plots of `SymPy` objects
to be agnostic as to a backend plotting package.  `SymPy` provides
recipes that allow symbolic expressions to be used where functions are
part of the `Plots` interface.
[See the help page for `sympy_plotting`.]

In particular, the following methods of `plot` are defined:

* `plot(ex::Sym, a, b)` will plot the expression of single variable over the interval `[a,b]`
* `plot!(ex::Sym, a, b)` will add to the current plot a plot of  the expression of single variable over the interval `[a,b]`
* `plot(exs::Vector{Sym}, a, b)` will plot each expression over `[a,b]`
* `plot(ex1, ex2, a, b)` will plot a parametric plot of the two expressions over the interval `[a,b]`.
* `contour(xs, ys, ex::Sym)` will make a contour plot of the expression of two variables over the grid specifed by the `xs` and `ys`.
* `surface(xs, ys, ex::Sym)` will make a surface plot of the expression of two variables over the grid specifed by the `xs` and `ys`.


For example:

```
x = symbols("x")
using Plots
pyplot()
#
plot(x^2 - 2, -2,2)
```

Or a parametric plot:

```
plot(sin(2x), cos(3x), 0, 4pi)
```

For plotting with other plotting packages, it is generally faster to
first call `lambdify` on the expression and then generate `y` values
with the resulting `Julia` function.

----

In addition, with `PyPlot` a few other plotting functions from `SymPy` are available from its interface to `MatplotLib`:

* `plot3d_parametric_surface(ex1::Sym, ex2::Sym, ex3::Sym), (uvar, a0,
  b0), (vvar, a1, b1))` -- make a surface plot of the expressions
  parameterized by the region `[a0,b0] x [a1,b1]`. The default region
  is `[-5,5]x[-5,5]` where the ordering of the variables is given by
  `free_symbols(ex)`.

* `plot_implicit(predictate, (xvar, a0, b0), (yvar, a1, b1))` -- make
an implicit equation plot of the expressions over the region `[a0,b0]
x [a1,b1]`. The default region is `[-5,5]x[-5,5]` where the ordering
of the variables is given by `free_symbols(ex)`.  To create predicates
from the variable, the functions `Lt`, `Le`, `Eq`, `Ge`, and `Gt` can
be used, as with `Lt(x*y, 1)`. For infix notation, unicode operators
can be used: `\ll<tab>`, `\leqq<tab>`, `\Equal<tab>`, `\geqq<tab>`, and
`\gg<tab>`. For example, `x*y ≪ 1`.  To combine terms, the unicode
`\vee<tab>` (for "or"), `\wedge<tab>` (for "and") can be used.


###  Example

```@example
using Plots, SymPy
@vars x
plot(sin(6x) + exp(sin(60x)), -1, 1)
savefig("sympy.svg"); nothing # hide
```

![](sympy.svg)


## Calculus

`SymPy` has many of the basic operations of calculus provided through a relatively small handful of functions.

### Limits

Limits are computed by the `limit` function which takes an expression, a variable and a value, and optionally a direction specified by either `dir="+"` or `dir="-"`.

For example, this shows Gauss was right:

```
limit(sin(x)/x, x, 0)
```

Alternatively, the second and third arguments can be specified as a pair:

```
limit(sin(x)/x, x=>0)
```

Limits at infinity are done by using `oo` for $\infty$:

```
limit((1+1/x)^x, x => oo)
```


This example computes what L'Hopital reportedly paid a Bernoulli for

```
a = symbols("a", positive=true)
ex = (sqrt(2a^3*x-x^4) - a*(a^2*x)^(1//3)) / (a - (a*x^3)^(1//4))
```

Substituting $x=a$ gives an indeterminate form:

```
ex(x=>a)         # or subs(ex, x, a)
```

We can see it is of the form $0/0$:

```
denom(ex)(x => a), numer(ex)(x => a)
```

And we get

```
limit(ex, x => a)
```

In a previous example, we defined `quad_approx`:

```
quad_approx
```

The limit as `h` goes to $0$ gives `1 - x^2/2`, as expected:

```
limit(quad_approx, h => 0)
```

#### Left and right limits

The limit is defined when both the left and right limits exist and are equal. But left and right limits can exist and not be equal. The `sign` function is $1$ for positive $x$, $-1$ for negative $x$ and $0$ when $x$ is 0. It should not have a limit at $0$:

```
limit(sign(x), x => 0)
```

Oops. Well, the left and right limits are different anyways:

```
limit(sign(x), x => 0, dir="-"), limit(sign(x), x => 0, dir="+")
```

(The `limit` function finds the *right* limit by default. To be
careful, either plot or check that both the left and right limit exist
and are equal.)


#### Numeric limits

The `limit` function uses the
[Gruntz](http://docs.sympy.org/latest/modules/series.html#the-gruntz-algorithm)
algorithm. It is far more reliable then simple numeric attempts at
limits. An example of Gruntz is the right limit at $0$ of the
function:

```
f(x) = 1/x^(log(log(log(log(1/x)))) - 1)
```

A numeric attempt might be done along these lines:

```
hs = [10.0^(-i) for i in 6:16]
ys = [f(h) for h in hs]
[hs ys]
```

With a values appearing to approach $0$. However, in fact these values will ultimately head  off to $\infty$:

```
limit(f(x), x, 0, dir="+")
```


### Derivatives

One *could* use limits to implement the definition of a derivative:

```
x, h = symbols("x,h")
f(x) = exp(x)*sin(x)
limit((f(x+h) - f(x)) / h, h, 0)
```

However, it would be pretty inefficient, as `SymPy` already does a great job with derivatives. The `diff` function implements this. The basic syntax is `diff(ex, x)` to find the first derivative in `x` of the expression in `ex`, or its generalization to $k$th derivatives with `diff(ex, x, k)`.

The same derivative computed above by a limit could be found with:

```
diff(f(x), x)
```

Similarly, we can compute other derivatives:

```
diff(x^x, x)
```

Or

```
diff(exp(-x^2), x, 2)
```

As an alternate to specifying the number of derivatives, multiple variables can be passed to `diff`:

```
diff(exp(-x^2), x, x, x)     # same as diff(..., x, 3)
```

This could include variables besides `x`.


The output is a simple expression, so `diff` can be composed with
other functions, such as `solve`. For example, here we find the
critical points where the derivative is $0$ of some rational function:

```
f(x) = (12x^2 - 1) / (x^3)
diff(f(x), x) |> solve
```


#### Partial derivatives

The `diff` function makes finding partial derivatives as easy as specifying the variable to differentiate in. This  example computes the mixed partials of an expression in `x` and `y`:

```
x,y = symbols("x,y")
ex = x^2*cos(y)
Sym[diff(ex,v1, v2) for v1 in [x,y], v2 in [x,y]]
```

The extra `Sym`, of the form `T[]`, helps `Julia` resolve the type of the output.

#### Unevaluated derivatives

The `Derivative` constructor provides unevaluated derivatives, useful with differential equations and the output for unknown functions. Here is an example:

```
ex = sympy.Derivative(exp(x*y), x, y, 2)
```

(The `y,2` is a replacement for `y,y` which makes higher order terms easier to type.) These expressions are evaluated with the `doit` method:

```
ex.doit()
```

#### Implicit derivatives

SymPy can be used to find derivatives of implicitly defined
functions. For example, the task of finding $dy/dx$ for the equation:

$$~
y^4 - x^4 -y^2 + 2x^2 = 0
~$$

As with the mathematical solution, the key is to treat one of the variables as depending on the other. In this case, we think of $y$ locally as a function of $x$. SymPy allows us to create symbolic functions, and we will use one to substitute in for `y`.


In SymPy, symbolic functions use the class name  "Function", but in `SymPy` we use `SymFunction` to avoid a name collision with one of `Julia`'s primary types. The constructor can be used as `SymFunction(:F)`:

```
F, G = SymFunction("F"), SymFunction("G")
```

We can call these functions, but we get a function expression:

```
F(x)
```

SymPy can differentiate symbolically, again with `diff`:

```
diff(F(x))
```

Of for symbolic functions the more natural `F'(x)`.

To get back to our problem, we have our expression:

```
x,y = symbols("x, y")
ex = y^4 - x^4 - y^2 + 2x^2
```

Now we substitute:

```
ex1 = ex(y=>F(x))
```

We want to differentiate "both" sides. As the right side is just $0$, there isn't anything to do here, but mentally keep track. As for the left we have:

```
ex2 = diff(ex1, x)
```

Now we collect terms and solve in terms of $F'(x)$

```
ex3 = solve(ex2, F'(x))[1]
```

Finally, we substitute back into the solution for $F(x)$:

```
ex4 = ex3(F(x) => y)
```

###### Example: A Norman Window

A classic calculus problem is to maximize the area of a
[Norman window](http://en.wiktionary.org/wiki/Norman_window) (in the
shape of a rectangle with a half circle atop) when the perimeter is
fixed to be $P \geq 0$.


Label the rectangle with $w$ and $h$ for width and height and then the
half circle has radius $r=w/2$. With this, we can see that the area is
$wh+(1/2)\pi r^2$ and the perimeter is $w + 2h + \pi r$. This gives:

```
w, h, P = symbols("w, h, P", nonnegative=true)
r = w/2
A = w*h + 1//2 * (pi * r^2)
p = w + 2h + pi*r
```

(There is a subtlety above, as m `1//2*pi*r^2` will lose exactness, as
the products will be done left to right, and `1//2*pi` will be
converted to an approximate floating point value before multiplying
`r^2`, as such we rewrite the terms. It may be easier to use `PI`
instead of `pi`.)

We want to solve for `h` from when `p=P` (our fixed value) and
substitute back into `A`. We solve `P-p==0`:

```
h0 =  solve(P-p, h)[1]
A1 = A(h => h0)
```

Now we note this is a parabola in `w`, so any maximum will be an
endpoint or the vertex, provided the leading term is negative.
The leading term can be found through:

```
sympy.Poly(A1, w).coeffs()
```

Or without using the `Poly` methods, we could do this:

```
collect(expand(A1), w).coeff(w^2)
```

Either way, the leading coefficient, $-1/2 - \pi/8$, is negative, so
the maximum can only happen at an endpoint or the vertex of the
parabola. Now we check that when $w=0$ (the left endpoint) the area is
$0$:

```
A1(w => 0)
```

The other endpoint is when $h=0$, or

```
b = solve((P-p)(h => 0), w)[1]
```

We will need to check the area at `b` and at the vertex.

To find the vertex, we can use calculus -- it will be when the derivative in `w` is $0$:

```
c = solve(diff(A1, w), w)[1]
```

The answer will be the larger of `A1` at `b` or `c`:

```
atb = A1(w => b)
atc = A1(w => c)
```

A simple comparison isn't revealing:

```
atc - atb
```

But after simplifying, we can see that this expression is positive if $P$ is:

```
simplify(atc - atb)
```

With this observation, we conclude the maximum area happens at `c` with area `atc`.

### Integrals

Integration is implemented in SymPy through the `integrate` function. There are two basic calls:
`integrate(f(x), x)` will find the indefinite integral ($\int f(x) dx$) and when endpoints are specified through `integrate(f(x), (x, a, b))` the definite integral will be found ($\int_a^b f(x) dx$). The special form `integrate(ex, x, a, b)` can be used for single integrals, but the specification through a tuple is needed for multiple integrals.

Basic integrals are implemented:

```
integrate(x^3, x)
```

Or in more generality:

```
n = symbols("n", real=true)
ex = integrate(x^n, x)
```

The output here is a *piecewise function*, performing a substitution will choose a branch in this case:

```
ex(n => 3)
```

Definite integrals are just as easy. Here is Archimedes' answer:

```
integrate(x^2, (x, 0, 1))
```


Tedious problems, such as those needing multiple integration-by-parts steps can be done easily:

```
integrate(x^5*sin(x), x)
```

The SymPy tutorial says:

> "`integrate` uses powerful algorithms that are always improving to compute both definite and indefinite integrals, including heuristic pattern matching type algorithms, a partial implementation of the Risch algorithm, and an algorithm using Meijer G-functions that is useful for computing integrals in terms of special functions, especially definite integrals."

The tutorial gives the following example:

```
f(x) = (x^4 + x^2 * exp(x) - x^2 - 2x*exp(x) - 2x - exp(x)) * exp(x) / ( (x-1)^2 * (x+1)^2 * (exp(x) + 1) )
f(x)
```

With indefinite integral:

```
integrate(f(x), x)
```

#### Multiple integrals

The `integrate` function uses a tuple, `(var, a, b)`, to specify the limits of a definite integral. This syntax lends itself readily to multiple integration.

For example, the following computes the integral of $xy$ over the unit square:

```
x, y = symbols("x,y")
integrate(x*y, (y, 0, 1), (x, 0, 1))
```

The innermost terms can depend on outer ones. For example, the following integrates $x^2y$ over the upper half of the unit circle:

```
integrate(x^2*y, (y, 0, sqrt(1 - x^2)), (x, -1, 1))
```


#### Unevaluated integrals

The `Integral` constructor can stage unevaluated integrals that will be evaluated by calling `doit`. It is also used when the output is unknown. This example comes from the tutorial:

```
integ = sympy.Integral(sin(x^2), x)
```

```
integ.doit()
```


### Taylor series

The `series` function can compute series expansions around a point to a specified order. For example,
the following command finds 4 terms of the series expansion of `exp(sin(x))` in `x` about $c=0$:

```
s1 = series(exp(sin(x)), x, 0, 4)
```

The coefficients are from the Taylor expansion ($a_i=f^{i}(c)/i!$). The
[big "O"](http://en.wikipedia.org/wiki/Big_O_notation) term indicates
that any other power is no bigger than a constant times $x^4$.


Consider what happens when we multiply series of different orders:

```
s2 = series(cos(exp(x)), x, 0, 6)
```

```
simplify(s1 * s2)
```

The big "O" term is $x^4$, as smaller order terms in `s2` are covered in this term. The big "O" notation is sometimes not desired, in which case the `removeO` function can be employed:

```
s1.removeO()
```



### Sums

`SymPy` can do sums, including some infinite ones. The `summation` function performs this task. For example, we have

```
i, n = symbols("i, n")
summation(i^2, (i, 1, n))
```

Like `Integrate` and `Derivative`, there is also a `Sum` function to stage the task until the `doit` function is called to initiate the sum.


Some famous sums can be computed:

```
sn = sympy.Sum(1/i^2, (i, 1, n))
sn.doit()
```

And from this a limit is available:

```
limit(sn.doit(), n, oo)
```

This would have also been possible through `summation(1/i^2, (i, 1, oo))`.

### Vector-valued functions

Julia makes constructing a vector of symbolic objects easy:

```
x,y = symbols("x,y")
v = [1,2,x]
w = [1,y,3]
```

The generic definitions of vector operations will work as expected with symbolic objects:

```
using LinearAlgebra
dot(v,w)
```

Or

```
cross(v,w)
```

Finding gradients can be done using a comprehension.

```
ex = x^2*y - x*y^2
Sym[diff(ex,var) for var in (x,y)]
```

The mixed partials is similarly done by passing two variables to differentiate in to `diff`:

```
Sym[diff(ex, v1, v2) for v1 in (x,y), v2 in (x,y)]
```

For this task, SymPy provides the `hessian` method:

```
hessian(ex, (x,y))
```

## Matrices

`Julia` has excellent infrastructure to work with generic matrices,
such as `Matrix{Sym}` objects (matrices with symbolic entries). As
well, SymPy has a class for matrices. `SymPy`, through `PyCall`, automatically maps mutable SymPy matrices into `Julia`n matrices of type `Array{Sym}`.



Constructing matrices with symbolic entries follows `Julia`'s conventions:

```
x,y = symbols("x,y")
M = [1 x; x 1]
```

Construction of symbolic matrices can *also* be done through the `Matrix` constructor, which must be qualified. It is passed a vector or row vectors but any symbolic values *must* be converted into `PyObject`s:

```
import PyCall: PyObject
A = sympy.Matrix([[1,PyObject(x)], [PyObject(x), 1]])
```

(otherwise, an entry like `[1,x]` will be mapped to a `Vector{Sym}` prior to passing to `sympy.Matrix` and the processing get's done differently, and not as desired.)

This is useful if copying SymPy examples, but otherwise unneccesary, these are immediately mapped into `Julia` arrays by `PyCall.
**Unless** an immutable array is desired, and then the `sympy.ImmutableMatrix` constructor is used. (Though it is *still* necessary to convert symbolic values to `PyObject`s.)



```
diagm(0=>ones(Sym, 5))
M^2
det(M)
```

Similarly,

```
A^2
```

We can call `Julia`'s generic matrix functions in the usual manner, e.g:

```
det(A)
```

We can also call SymPy's matrix methods using the dot-call syntax:


```
A.det()
```


Occasionally, the SymPy method has more content:

```
eigvecs(M)
```

As compared to SymPy's `eigenvects` which yields:

```
A.eigenvects()
```

(This is a bit misleading, as the generic `eigvecs` fails on `M`, so the value is basically just repackaged from `A.eigenvects()`.)

This example from the tutorial shows the `nullspace` function:

```
A = Sym[1 2 3 0 0; 4 10 0 0 1]
vs = A.nullspace()
```

And this shows that they are indeed in the null space of `M`:

```
[A*vs[i] for i in 1:3]
```

Symbolic expressions can be included in the matrices:

```
A = [1 x; x 1]
P, D = A.diagonalize()  # M = PDP^-1
A - P*D*inv(P)
```


## Differential equations

SymPy has facilities for solving ordinary differential
[equations](http://docs.sympy.org/latest/modules/solvers/ode.html). The
key is to create a symbolic function expression using
`SymFunction`. Again, this may be done through:

```
F = SymFunction("F")
```

With this, we can  construct a  differential equation. Following the SymPy tutorial, we solve $f''(x) - 2f'(x) + f(x) = \sin(x)$:

```
diffeq = Eq(diff(F(x), x, 2) - 2*diff(F(x)) + F(x), sin(x))
```


With this, we just need the `dsolve` function. This is called as `dsolve(eq)` or `dsolve(eq, F(x))`:

```
ex = dsolve(diffeq, F(x))
```


The `dsolve` function in SymPy has an extensive list of named
arguments to control the underlying algorithm. These can be passed
through with the appropriate keyword arguments. (To use SymPy's `ics` argument, the `sympy.dsolve` method must be called directly.)

More clearly, the `SymFunction` objects have the `'` method defined to
find a derivative, so the above could also have been:

```
diffeq = F''(x) - 2F'(x) + F(x) - sin(x)
sympy.dsolve(diffeq, F(x))
```

This solution has two constants, $C_1$ and $C_2$, that would be found from initial conditions. Say we know $F(0)=0$ and $F'(0)=1$, can we find the constants? To work with the returned expression, it is most convenient to get just the right hand side. The `rhs` method will return the right-hand side of a relation:

```
ex1 = ex.rhs()
```

(The
[args](http://docs.sympy.org/dev/modules/core.html#sympy.core.basic.Basic.args)
function also can be used to break up the expression into parts.)

With this, we can solve for `C1` through substituting in $0$ for $x$:

```
solve(ex1(x => 0), Sym("C1"))
```

We see that $C1=-1/2$, which we substitute in:

```
ex2 = ex1(Sym("C1") => -1//2)
```

We know that $F'(0)=1$ now, so we solve for `C2` through

```
solve( diff(ex2, x)(x => 0) - 1, Sym("C2") )
```

This gives `C2=3/2`. Again we substitute in to get our answer:

```
ex3 = ex2(Sym("C2") => 3//2)
```



###### Example

We do one more example, this one borrowed from [here](http://nbviewer.ipython.org/github/garth-wells/IA-maths-Ipython/blob/master/notebooks/Lecture1.ipynb).

> Find the variation of speed with time of a parachutist subject to a drag force of $k\cdot v^2$.

The equation is

$$~
\frac{m}{k} \frac{dv}{dt} = \alpha^2 - v^2.
~$$

We proceed through:

```
t, m,k,alpha = symbols("t,m,k,alpha")
v = SymFunction("v")
ex = Eq( (m/k)*v'(t), alpha^2 - v(t)^2 )
```

We can "classify" this ODE with the method `classify_ode` function.

```
sympy.classify_ode(ex)
```

It is linear, but not solvable. Proceeding with `dsolve` gives:

```
dsolve(ex, v(t))
```


### Initial Value Problems

Solving an initial value problem can be a bit tedious with `SymPy`.
The first example shows the steps. This is because the `ics` argument
for `sympy.dsolve` only works for a few types of equations. These do not
include, by default, the familiar "book" examples, such as $y'(x) =
a\cdot y(x)$.

To work around this, `SymPy.jl` extends the function `dsolve` to allow
a specification of the initial conditions when solving.  Each initial condition is specified with 3-tuple. For example, `v(t0)=v0` is specified with `(v, t0, v0)`.  The conditions on the values functions may use `v`, `v'`, ...
To illustrate, we follow an example from
[Wolfram](https://reference.wolfram.com/language/tutorial/DSolveLinearBVPs.html).

```
y = SymFunction("y")
a, x = symbols("a,x")
eqn = y'(x) - 3*x*y(x) - 1
```



We solve the initial value problem with $y(0) = 4$ as follows:

```
x0, y0 = 0, 4
out = dsolve(eqn, x, ics = (y, x0, y0))
```

Verifying this requires combining some operations:

```
u = out.rhs()
diff(u, x) - 3*x*u - 1
```

To solve with a general initial condition is similar:

```
x0, y0 = 0, a
out = dsolve(eqn, x, ics=(y, x0, y0))
```


To plot this over a range of values for `a` we have:

```
as = -2:0.6:2
ex = out.rhs()
p = plot(ex(a=>as[1]), -1.8, 1.8, ylims=(-4, 4))
[plot!(p, ex(a=>i), -1.8, 1.8, ylims=(-4, 4)) for i in as[2:end]]
p
```

The comment from the example is "This plots several integral curves of the equation for different values of $a$. The plot shows that the solutions have an inflection point if the parameter  lies between $-1$ and $1$ , while a global maximum or minimum arises for other values of $a$."


##### Example

We continue with another example from the Wolfram documentation, that
of solving $y'' + 5y' + 6y=0$ with values prescribed for both $y$ and
$y'$ at $x_0=0$.

```
y = SymFunction("y")
x = symbols("x")
eqn = y''(x) + 5y'(x) + 6y(x)
```

To solve with $y(0) = 1$ and $y'(0) = 1$ we have:

```
out = dsolve(eqn, x, ics=((y, 0, 1), (y', 0, 1)))
```

(That is we combine *all* initial conditions into a tuple.)

To make a plot, we only need the right-hand-side of the answer:

```
plot(out.rhs(), -1/3, 2)
```

##### Example

Boundary value problems can be solved for, as well, through a similar
syntax. Continuing with examples from the
[Wolfram](https://reference.wolfram.com/language/tutorial/DSolveLinearBVPs.html)
page, we solve $y''(x) +y(x) = e^x$ over $[0,1]$ with conditions
$y(0)=1$, $y(1) = 1/2$:

```
eqn = y''(x) + y(x) - exp(x)
dsolve(eqn, x, ics=((y, 0, 1), (y, 1, 1//2)))
```
