# A SymPy tutorial

This tutorial provides an introduction to using `SymPy` within `Julia`.
It owes an enormous debt to the tutorial for using SymPy within Python which may be found
[here](http://docs.sympy.org/dev/tutorial/index.html). The overall structure and many examples are taken from that work, with adjustments and additions to illustrate the differences due to using `SymPy` within `Julia`.

This tutorial can be read as an `IJulia` notebook [here](http://nbviewer.ipython.org/github/jverzani/SymPy.jl/blob/master/examples/tutorial.ipynb).

After installing `SymPy`, which is discussed in the package's `README`
file, we must first load it into `Julia` with the standard command
`using`:


```
using SymPy
```

The start up time is a bit lengthy.

## Symbols

At the core of `SymPy` is the introduction of symbolic variables that differ quite a bit from `Julia`'s variables. Symbolic variables do not immediately evaluate to a value, rather the "symbolicness" propagates when interacted with. To keep things manageable, SymPy does some simplifications along the way.

Symbolic expressions are primarily of the `Sym` type and can be constructed in the standard way:

```
x = Sym("x")
```

This creates a symbolic object `x`, which can be manipulated through further function calls.


There are two macros that make creating multiple variables a bit less typing. The `@vars` macro will create variables in the *Main workspace*, so no assignment is necessary. The `@syms` macro will return newly defined symbolic variables. As these are macros, the arguments need not be separated by commas.

```
@vars a b c
a,b,c = @syms a,b,c
```


### Assumptions

Finally, there is the `symbols` constructor. With `symbols` it is
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



### Special constants

`Julia` has its math constants, like `pi` and `e`, `SymPy` as well. A few of these have `Julia` counterparts provided by `SymPy`. For example, these three constants are defined (where `oo` is for infinity):

```
PI, E, oo
```


Numeric values themselves can be symbolic. This example shows the
difference. The first `asin` call dispatches to `Julia`'s `asin`
function, the second to `SymPy`'s:

```
asin(1), asin(Sym(1))
```

## Substitution

SymPy provides a means to substitute values in for the symbolic expressions. The specification requires an expression, a variable in the expression to substitute in for, and a new value. For example, this is one way to make a polynomial in a new variable:

```
x, y = symbols("x,y")
ex = x^2 + 2x + 1
subs(ex, x, y)
```

Substitution can also be numeric:

```
subs(ex, x, 0)
```

The output has no free variables, but is still symbolic.

Expressions with more than one variable can have multiple substitutions, where each is expressed as a tuple:

```
x,y,z = symbols("x,y,z")
ex = x + y + z
subs(ex, (x,1), (y,pi))      
```

Or keyword arguments can be used to refer to the variable to substitute:

```
subs(ex, x=1, y=pi)
```

When used with the pipeline operator, `|>`, there is a curried form that allows the expression to be implicit:

```
ex |> subs(x, 1)
```

As `subs` is very similar in spirit to `Julia`'s `replace` function, an alias is provided:

```
ex |> replace(y, pi)
```

### Using function notation to substitute

For `julia` version 0.4 and onward, changes to the language allow for `subs` to be replaced by a function-like notation:

```
if VERSION >= v"0.4.0-dev"
  x(100)
  ex(y=pi)
  ex(x=1, y=pi)
end
```

The first example above uses the internal `get_free_symbols` function
to order the symbols found in the expression. These may not be as
expected. This style should really only be used when there is just one
free symbol in the expression.

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
N(PI)  # floating-point value
```

Whereas, this may look the same, it is still symbolic:

```
evalf(PI)
```

Both allow for a precision argument to be passed through the second argument. This is how 30 digits of $\pi$ can be extracted:

```
N(PI, 30)
```

Here `N` produces a `BigFloat` with a precision to match (basically) the specified number of digits. Whereas

```
evalf(PI, 30)
```

leaves the value as a symbolic object with 30 digits of accuracy.

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
but the two latter expressions do not.


`SymPy` makes it very easy to work with polynomial and rational expressions. First we create some variables:

```
x,y,z = symbols("x, y, z")
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
expand(prod([(x-i) for i in 1:5]))
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
x,y = symbols("x,y", nonnegative=true)
a = symbols("a", real=true)
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

Students may forget, but the `trigsimp` function is, of course, course aware of the double angle formulas:

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

The `coeff(ex, monom)` function will return the corresponding coefficient of the monomial:

```
coeff(p, x^2) # a 
coeff(p, x)   # b
```

The constant can be found through substitution:

```
subs(p, x, 0) # c
```

Though one could use some trick like this to find all the coefficients:

```
[[coeff(p, x^i) for i in N(degree(p)):-1:1], subs(p,x,0)]
```

that is cumbersome, at best. SymPy has a function `coeffs`, but it is defined for polynomial types, so will fail on `p`:


```
coeffs(p) # fails
```

Polynomials are a special class in SymPy and must be constructed. The `Poly` constructor can be used. As there is more than one free variable in `p`, we specify the variable `x` below:

```
q = Poly(p, x)
coeffs(q)
```


## Polynomial roots: solve, real_roots, polyroots, nroots

SymPy provides functions to find the roots of a polynomial. In
general, a polynomial with real coefficients of degree $n$ will have
$n$ roots when multiplicities and complex roots are accounted for. The
number or real roots is consequently between $0$ and $n$.

For a *univariate* polynomial expression (a single variable), the real
roots, when available, are returned by `real_roots`. For example,

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

This particular function is not exported (as it conflicts with the
`roots` function from `Polynomials` and `Roots`) but we can still
access it using `p[:roots]()` or its alias `polyroots`.

> Indexing with a symbol. When a symbolic expression is indexed by a
> symbol it returns a function which maps to a corresponding SymPy
> function. For example, "p[:roots](args...)" will call `roots(p,
> args...)` within SymPy. For methods of SymPy objects, the same is
> true, so if `roots` were a class method, then the call would resolve
> to `p.roots(args...)`.

The output of calling `roots` will be a dictionary whose keys are the roots and values the multiplicity.

```
polyroots(p)
```

When exact answers are not provided, the `:roots` call is contentless:

```
p = x^5 - x + 1
polyroots(p)
```

Calling `solve` seems to produce very little as well:

```
rts = solve(p)
```

But in fact, `rts` contains lots of information. We can extract numeric values quite easily with `N`:

```
[N(r) for r in rts]     # or map(N, rts)
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

The `solve` function has limits. For example, there is no symbolic solution here:

```
solve(cos(x) - x)
```

For such, a numeric method would be needed.

Though it can't solve everything, the `solve` function can also solve
equations of a more general type. For example, here it is used to
derive the quadratic equation:

```
a,b,c  = symbols("a,b,c", real=true)
p = a*x^2 + b*x + c
solve(p, x)
```

The extra argument `x` is passed to `solve` so that `solve` knows
which variable to solve for. If not given, `solve` tries to find a
solution with all the free variables, which in this case is not
helpful:

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

Plugging the solutions into the equations to check is a bit
cumbersome, as the keys of the dictionary that is returned are
strings, but functions like `subs` prefer the variables. Here is one
workaround:

```
vars = [x,y]
vals = [d[string(var)] for var in vars]
[subs(ex, zip(vars, vals)...) for ex in exs]
```

In the previous example, the system had two equations and two
unknowns. When that is not the case, one can specify the variables to
solve for as a vector. In this example, we find a quadratic polynomial
that approximates $\cos(x)$ near $0$:

```
a,b,c,h = symbols("a,b,c,h", real=true)
p = a*x^2 + b*x + c
fn = cos
exs = [fn(0*h)-subs(p,x,0), fn(h)-subs(p,x,h), fn(2h)-subs(p,x,2h)]
d = solve(exs, [a,b,c])
```

Again, a dictionary is returned. The polynomial itself can be found by
substituting back in for `a`, `b`, and `c`:

```
vars=[a,b,c]
vals = [d[string(var)] for var in vars]
quad_approx = subs(p, zip(vars, vals)...)
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
`\ll[tab]`, `\le[tab]`, `\Equal[tab]`, `\ge[tab]`, `\gg[tab]` and
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
to be agnostic as to a backend plotting package.  `SymPy` loads the
`Plots` package and extends its `plot` and `plot!`
methods. [See the help page for `sympy_plotting`.]

In particular, the following methods of `plot` are defined:

* `plot(ex::Sym, a, b)` will plot the expression of single variable over the interval `[a,b]`
* `plot!(ex::Sym, a, b)` will add to the current plot a plot of  the expression of single variable over the interval `[a,b]`
* `plot(exs::Vector{Sym}, a, b)` will plot each expression over `[a,b]`
* `plot(ex1, ex2, a, b)` will plot a parametric plot of the two expressions over the interval `[a,b]`.

There are corresponding `plot!` methods.

The basic idea, is where there is an interface in `Plots` for
functions, a symbolic expression may be passed in.  Keyword arguments
are passed onto the `plot` function of the `Plots` package.

As an alternative to the above style for parametric plots, we add
`parametricplot(ex1, ex2, a, b)` and `plot((ex1, ex2), a, b)`.

When `Plots` adds contour plots and plots of vector fields, these will be added to `SymPy`.

----

In addition, within Python, SymPy has several plotting features that work with Matplotlib. Many of these are available
when the `:pyplot` backend end for `Plots` is used (`backend(:pyplot)`). These methods are only available *after* `PyPlot` is loaded. If this done through `Plots`, then this happens after an initial call to `plot`. If this is done through `using PyPlot`, then the `plot` method will be ambiguous with `PyPlot`'s and must be qualified, as in `SymPy.plot`.


* `plot((ex1, ex2, ex3), a, b)` --  plot a 3D parametric plot of the expressions over `[a,b]`. (Also `parametricplot`.)

* `contour(ex, (xvar, a0, b0), (yvar, a1, b1))` -- make a contourplot
  of the expression of two variables over the region `[a0,b0] x
  [a1,b1]`. The default region is `[-5,5]x[-5,5]` where the ordering
  of the variables is given by `free_symbols(ex)`. This name may
  change when `Plots` adds contour plots. There is also `contour3D` to
  add a third dimension to the plot.

* `quiver(exs::Vector{Sym}, (xvar, a0, b0), (yvar, a1, b1))` -- make a
vector plot of the expressions over a grid within `[a0,b0] x
[a1,b1]`. The default region is `[-5,5]x[-5,5]` where the ordering of
the variables is given by `free_symbols(ex)`.  This name may change
when `Plots` adds vector field plots. Currently, there is a
`vectorplot` alias in anticipation of that.

There are also 3 dimesional plots avaiable

* `plot_surface(ex::Sym, (xvar, a0, b0), (yvar, a1, b1))` -- make a
  surface plot of the expressions over a grid within `[a0,b0] x
  [a1,b1]`. The default region is `[-5,5]x[-5,5]` where the ordering
  of the variables is given by `free_symbols(ex)`.

The plotting features of SymPy add some functions to Matplotlib and we
copy these over:

* `plot_parametric_surface(ex1::Sym, ex2::Sym, ex3::Sym), (uvar, a0,
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
can be used: `\ll<tab>`, `\le<tab>`, `\Equal<tab>`, `\ge<tab>`, and
`\gg<tab>`. For example, `x*y ≪ 1`.  To combine terms, the unicode
`\vee<tab>` (for "or"), `\wedge<tab>` (for "and") can be used.

----

Underlying the plotting commands are two simple things. First, to
easily create a function from a symbolic expression, the pattern
`convert(Function, ex)` can be used. This is useful for expression
containings a single variable. For others, the pattern
`Float64[subs(ex, (xvar, x), (yvar),y)) for x in xs, y in ys]` is
useful. 

## Calculus

`SymPy` has many of the basic operations of calculus provided through a relatively small handful of functions.

### Limits

Limits are computed by the `limit` function which takes an expression, a variable and a value, and optionally a direction specified by either `dir="+"` or `dir="-"`.

For example, this shows Gauss was right:

```
limit(sin(x)/x, x, 0)
```

Limits at infinity are done by using `oo` for $\infty$:

```
limit((1+1/x)^x, x, oo)
```


This example computes what L'Hopital reportedly paid a Bernoulli for

```
a = symbols("a", positive=true)
ex = (sqrt(2a^3*x-x^4) - a*(a^2*x)^(1//3)) / (a - (a*x^3)^(1//4))
```

The limit as $x$ goes to $a$ gives an indeterminate expression:

```
subs(ex, x, a)
```

we can see it is of the form $0/0$:

```
subs(denom(ex), x, a), subs(numer(ex), x, a)
```

And we get

```
limit(ex, x, a)
```

In a previous example, we defined `quad_approx`. The limit as `h` goes to $0$ gives `1 - x^2/2`, as expected:

```
limit(quad_approx, h, 0)
```

#### Left and right limits

The limit is defined when both the left and right limits exist and are equal. But left and right limits can exist and not be equal. The `sign` function is $1$ for positive $x$, $-1$ for negative $x$ and $0$ when $x$ is 0. It should not have a limit at $0$:

```
limit(sign(x), x, 0)
```

Oops. Well, the left and right limits are different anyways:

```
limit(sign(x), x, 0, dir="-"), limit(sign(x), x, 0, dir="+")
```

(The `limit` function finds the *right* limit by default. To be
careful, either plot or check that both the left and right limit exist
and are equal.)

#### Operator interface


For univariate functions there is an "operator" interface, where we pass a function object as the first argument and the value for `c` as the second (the variable is implicit, as `f` has only one).

```
f(x) = sin(5x)/x
limit(f, 0)
```

#### Numeric limits

The `limit` function uses the
[Gruntz](http://docs.sympy.org/latest/modules/series.html#the-gruntz-algorithm)
algorithm. It is far more reliable then simple numeric attempts at
integration. An example of Gruntz is the right limit at $0$ of the
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


The output is a simple expression, so `diff` can be composed with other functions, such as `solve`. For example, here we find the critical points of a rational function:

```
f(x) = (12x^2 - 1) / (x^3)
diff(f(x), x) |> solve
```

#### Operator version

`SymPy` provides an "operator" version of `diff` for univariate functions for convenience (`diff(f::Function,k=1)=diff(f(x),x,k)`):

```
f(x) = exp(x)*cos(x)
diff(f, 2)
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

The `Derivative` function provides unevaluated derivatives, useful with differential equations and the output for unknown functions. Here is an example:

```
ex = Derivative(exp(x*y), x, y, 2)
```

(The `y,2` is a replacement for `y,y` which makes higher order terms easier to type.) These expressions are evaluated with `doit`:

```
doit(ex)
```

#### Implicit derivatives

SymPy can be used to find derivatives of implicitly defined
functions. For example, the task of finding $dy/dx$ for the equation:

$$~
y^4 - x^4 -y^2 + 2x^2 = 0
~$$

As with the mathematical solution, the key is to treat one of the variables as depending on the other. In this case, we think of $y$ locally as a function of $x$. SymPy allows us to create symbolic functions, and we will use one to substitute in for `y`.


In SymPy, symbolic functions use the class name  "Function", but in `SymPy` we use `SymFunction` to avoid a name collision with one of `Julia`'s primary types. The constructor can be used as `SymFunction(:F)`, or more clearly passed to the `symbols` function through the `cls` keyword:

```
F, G = symbols("F, G", cls=SymFunction)
```

We can call these functions, but we get a function expression:

```
F(x)
```

SymPy can differentiate symbolically, again with `diff`:

```
diff(F(x))
```

To get back to our problem, we have our expression:

```
x,y = symbols("x, y")
ex = y^4 - x^4 - y^2 + 2x^2
```

Now we substitute:

```
ex1 = subs(ex, y, F(x))
```

We want to differentiate "both" sides. As the right side is just $0$, there isn't anything to do here, but mentally keep track. As for the left we have:

```
ex2 = diff(ex1, x)
```

Now we collect terms and solve in terms of $F'(x)$

```
ex3 = solve(ex2, diff(F(x)))[1]
```

Finally, we substitute back into the solution for $F(x)$:

```
ex4 = subs(ex3, F(x), y)
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
A1 = subs(A, h, h0)
```

Now we note this is a parabola in `w`, so any maximum will be an
endpoint or the vertex, provided the leading term is negative. 
The leading term can be found through:

```
coeffs(Poly(A1, w))
```

Or without using the `Poly` methods, we could do this:

```
coeff(collect(expand(A1), w), w^2)
```

Either way, the leading coefficient, $-1/2 - \pi/8$, is negative, so
the maximum can only happen at an endpoint or the vertex of the
parabola. Now we check that when $w=0$ (the left endpoint) the area is
$0$:

```
subs(A1, w, 0)
```

The other endpoint is when $h=0$, or

```
b = solve(subs(P-p, h, 0), w)[1]
```

We will need to check the area at `b` and at the vertex.

To find the vertex, we can use calculus -- it will be when the derivative in `w` is $0$:

```
c = solve(diff(A1, w), w)[1]
```

The answer will be the larger of `A1` at `b` or `c`:

```
atb = subs(A1, w, b)
atc = subs(A1, w, c)
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
subs(ex, n, 3)
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

"`integrate` uses powerful algorithms that are always improving to compute both definite and indefinite integrals, including heuristic pattern matching type algorithms, a partial implementation of the Risch algorithm, and an algorithm using Meijer G-functions that is useful for computing integrals in terms of special functions, especially definite integrals."

The tutorial gives the following example:

```
f(x) = (x^4 + x^2 * exp(x) - x^2 - 2x*exp(x) - 2x - exp(x)) * exp(x) / ( (x-1)^2 * (x+1)^2 * (exp(x) + 1) )
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

The `Integral` function can stage unevaluated integrals that will be evaluated by calling `doit`. It is also used when the output is unknown. This example comes from the tutorial:

```
integ = Integral(sin(x^2), x)
```

```
doit(integ)
```

#### Operator version
For convenience, for univariate functions there is a convenience wrapper so that the operator styles -- `integrate(f)` and `integrate(f, a, b)` -- will perform the integrations.

```
f(x) = exp(x) * cos(x)
integrate(f)
```

Or

```
integrate(sin, 0, pi)
```

### Taylor series

The `series` function can compute series expansions around a point to a specified order. For example,
the following command finds 4 terms of the series expansion os `exp(sin(x))` in `x` about $c=0$:

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
removeO(s1)
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
sn = Sum(1/i^2, (i, 1, n))
doit(sn)
```

And from this a limit is available:

```
limit(doit(sn), n, oo)
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
dot(v,w)
```

Or

```
cross(v,w)
```

Finding gradients can be done using a comprehension.

```
ex = x^2*y - x*y^2
Sym[diff(ex,var) for var in [x,y]]
```

The mixed partials is similarly done by passing two variables to differentiate in to `diff`:

```
Sym[diff(ex, v1, v2) for v1 in [x,y], v2 in [x,y]]
```

For this task, SymPy provides the `hessian` function:

```
hessian(ex)
```

(When there are symbolic parameters, the free variables are specified as a vector, as in `hessian(ex, vars)`.)

## Matrices

SymPy has a special class to work with matrices, as does `Julia`. With
`SymPy`, matrices are just `Julia`n matrices with symbolic
entries. The conversion to matrices that SymPy knows about is
primarily handled in the background, though, if need be,
`convert(SymMatrix, M)` can be used.

Constructing matrices then follows `Julia`'s conventions:

```
x,y = symbols("x,y")
M = [1 x; x 1]
```

As much as possible, generic `Julia` functions are utilized:

```
diagm(ones(Sym, 5))
M^2
det(M)
```

Occasionally, the SymPy method has more content:

```
rref(M)
```

As compared to SymPy's [rref](http://docs.sympy.org/dev/tutorial/matrices.html#rref) which has a second list of indices used for pivoting:

```
M[:rref]()
```

(Similarly, `eigvecs(M)` is less informative than `M[:eigenvecs]()`.)

This example from the tutorial shows the `nullspace` function:

```
M = [one(Sym) 2 3 0 0; 4 10 0 0 1]
vs = nullspace(M)
```

And this shows that they are indeed in the null space of `M`:

```
[M*vs[i] for i in 1:3]
```

Symbolic expressions can be included in the matrices:

```
M = [1 x; x 1]
P, D = diagonalize(M)  # M = PDP^-1
D
```


## Differential equations

SymPy has facilities for solving ordinary differential [equations](http://docs.sympy.org/latest/modules/solvers/ode.html). The key is to create a symbolic function expression using the `SymFunction` class. Again, this may be done through:

```
F = symbols("F", cls=SymFunction)
```

With this, we can  construct a  differential equation. Following the SymPy tutorial, we solve $f''(x) - 2f'(x) + f(x) = \sin(x)$:

```
diffeq = Eq(diff(F(x), x, 2) - 2*diff(F(x)) + F(x), sin(x))
```


With this, we just need the `dsolve` function. This is called as `dsolve(eq, variable)`:

```
ex = dsolve(diffeq, F(x))
```

This solution has two constants, $C_1$ and $C_2$, that would be found from initial conditions. Say we know $F(0)=0$ and $F'(0)=1$, can we find the constants? To work with the returned expression, it is most convenient to get just the right hand side. The `rhs` function will return the right-hand side of a relation:

```
ex1 = rhs(ex)
```

(The [args](http://docs.sympy.org/dev/modules/core.html#sympy.core.basic.Basic.args) function also can be used to break up the expression into parts.)

With this, we can solve for `C1` through substituting in $0$ for $x$:

```
solve(subs(ex1, x, 0), Sym("C1"))
```

We see that $C1=-1/2$, which we substitute in:

```
ex2 = subs(ex1, Sym("C1"), -1//2)
```

We know that $F'(0)=1$ now, so we solve for `C2` through

```
solve( subs(diff(ex2, x), x, 0) - 1, Sym("C2") )
```

This gives `C2=3/2`. Again we substitute in to get our answer:

```
ex3 = subs(ex2, Sym("C2"), 3//2)
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
v = symbols("v", cls=SymFunction)
ex = Eq( (m/k)*diff(v(t),t), alpha^2 - v(t)^2 )
```

We can "classify" this ODE with the method `classify_ode`. As this is not exported, we call it using indexing:

```
ex[:classify_ode]()
```

It is linear, but not solvable. Proceeding with `dsolve` gives:

```
dsolve(ex, v(t))
```
