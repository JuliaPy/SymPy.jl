# A SymPy introduction

This document provides an introduction to using `SymPy` within `Julia`.
It owes an enormous debt to the tutorial for using SymPy within Python which may be found
[here](http://docs.sympy.org/dev/tutorial/index.html). The overall structure and many examples are taken from that work, with adjustments and additions to illustrate the differences due to using `SymPy` within `Julia`.


After installing `SymPy`, which is discussed in the package's `README`
file, we must first load it into `Julia` with the standard command
`using`:


```@setup introduction
using SymPy
sympy.init_printing(use_latex=true)
```

```jldoctest introduction
julia> using SymPy

```

The start up time is a bit lengthy (~4s).

## Symbols

At the core of `SymPy` is the introduction of symbolic variables that
differ quite a bit from `Julia`'s variables. Symbolic variables do not
immediately evaluate to a value, rather the "symbolicness" propagates
when interacted with. To keep things manageable, SymPy does some
simplifications along the way.

Symbolic expressions are primarily of the `Sym` type and can be constructed in the standard way:

```jldoctest introduction
julia> x = Sym("x")
x

```

This creates a symbolic object `x`, which can be manipulated through further function calls.


There are the `@syms` and `@vars` macros that makes creating multiple variables a
bit less typing, as it creates variables in the local scope -- no
assignment is necessary. Compare these similar ways to create symbolic
variables:

```jldoctest introduction
julia> @syms a b c
(a, b, c)

julia> @vars u v w
(u, v, w)

julia> a,b,c = Sym("a,b,c")
(a, b, c)

```

Here are two ways to make related variables:

```jldoctest introduction
julia> @syms xs[1:5]
(Sym[xs₁, xs₂, xs₃, xs₄, xs₅],)

julia> ys = [Sym("y$i") for i in 1:5]
julia> 5-element Vector{Sym}:
 y₁
 y₂
 y₃
 y₄
 y₅
5-element Vector{Sym}:
 y₁
 y₂
 y₃
 y₄
 y₅
```

The former much more succinct, but the latter pattern of use when the number of terms is a variable.


The `@syms` macro is recommended, and will be modeled in the following, as it makes the specification of assumptions and symbolic functions more natural.

### Assumptions

Finally, there is the `symbols` constructor for producing symbolic objects. With `symbols` it is
possible to pass assumptions onto the variables. A list of possible
assumptions is
[here](http://docs.sympy.org/dev/modules/core.html#module-sympy.core.assumptions). Some
examples are:

```jldoctest introduction
julia> u = symbols("u")
u

julia> x = symbols("x", real=true)
x

julia> y1, y2 = symbols("y1, y2", positive=true)
(y1, y2)

julia> alpha = symbols("alpha", integer=true, positive=true)
α

```

As seen, the `symbols` function can be used to make one or more variables with zero, one or more assumptions.

We jump ahead for a second to illustrate, but here we see that `solve` will respect these assumptions, by failing to find solutions to these equations:

```jldoctest introduction
julia> solve(x^2 + 1)   # ±i are not real
Any[]

```

```jldoctest introduction
julia> solve(y1 + 1)    # -1 is not positive
Any[]

```

The `@syms` macro allows annotations, akin to type annotations, to specify assumptions on new variables:

```jldoctest introduction
julia> @syms u1::positive u2::positive
(u1, u2)

julia> solve(u1 + u2)  # empty, though solving u1 - u2 is not.
Any[]
```

The `@vars` macro can also have assumptions passed in as follows; the assumptions apply to each variable.

```jldoctest introduction
julia> @vars u1 u2 positive=true
(u1, u2)

julia> solve(u1 + u2)  # empty, though solving u1 - u2 is not.
Any[]

```

Additionally you can rename arguments using pair notation:
```
julia> @syms a1=>"α₁" a2=>"α₂"
(α₁, α₂)
```

In this example, the Julia variables `a1` and `a2` are defined to store SymPy
symbols with the "pretty" names `α₁` and `α₂` respectively.

As can be seen, there are several ways to create symbolic values, but
the recommended way is to use `@syms`. One caveat is that one can't
use `Sym` to create a variable from a function name in Base.

### Special constants

`Julia` has its math constants, like `pi` and `e`, `SymPy` as well. A few of these have `Julia` counterparts provided by `SymPy`. For example, these two constants are defined (where `oo` is for infinity):

```jldoctest introduction
julia> PI,  oo
(pi, oo)

```

(The pretty printing of SymPy objects does not work for tuples.)

Numeric values themselves can be symbolic. This example shows the
difference. The first `asin` call dispatches to `Julia`'s `asin`
function, the second to `SymPy`'s:

```jldoctest introduction
julia> [asin(1), asin(Sym(1))]
2-element Vector{Sym}:
 1.57079632679490
             pi/2

```

## Substitution

SymPy provides a means to substitute values in for the symbolic expressions. The specification requires an expression, a variable in the expression to substitute in for, and a new value. For example, this is one way to make a polynomial in a new variable:

```jldoctest introduction
julia> @syms x y
(x, y)

julia> ex = x^2 + 2x + 1
 2
x  + 2⋅x + 1

julia> ex.subs(x, y)
 2
y  + 2⋅y + 1

```


Substitution can also be numeric:

```jldoctest introduction
julia> ex.subs(x, 0)
1


```

The output has no free variables, but is still symbolic.

Expressions with more than one variable can have multiple substitutions, where each is expressed as a tuple:

```jldoctest introduction
julia> @syms x,y,z
(x, y, z)

julia> ex = x + y + z
x + y + z

julia> ex.subs((x,1), (y,pi))
x + y + z

```

!!! note

    The calling pattern for `subs` is different from a typical `Julia` function call. The `subs` call is `object.method(arguments)` whereas a more "`Julia`n" function call is `method(objects, other objects....)`, as `Julia` offers multiple dispatch of methods. `SymPy` uses the Python calling method, adding in `Julia`n style when appropriate for generic usage within `Julia`. `SymPy` imports most all generic functions from the underlying `sympy` module and specializes them on a symbolic first argument.

    For `subs`, the simple substitution `ex.object(x,a)` is similar to simple function evaluation, so `Julia`'s call notation will work. To specify the pairing off of `x` and `a`, the `=>`  pairs notation is used.

This calling style will be equivalent to the last:

```jldoctest introduction
julia> ex(x=>1, y=>pi)
z + 1 + π

```

A straight call is also possble, where the order of the variables is determined by `free_symbols`:

```jldoctest introduction
julia> ex(1, pi)
z + 1 + π

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

```jldoctest introduction
julia> N(PI)  # converts to underlying pi irrational
π = 3.1415926535897...

```

Whereas, `evalf` will produce a symbolic numeric value:

```jldoctest introduction
julia> (PI).evalf()
3.14159265358979

```


The `evalf` call allows for a precision argument to be passed through the second argument. This is how 30 digits of $\pi$ can be extracted:

```jldoctest introduction
julia> PI.evalf(30)
3.14159265358979323846264338328

```

This is a SymPy, symbolic number, not a `Julia` object. Composing with `N`

```jldoctest introduction
julia> N(PI.evalf(30))
3.141592653589793238462643383279999999999999999999999999999999999999999999999985

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

```jldoctest introduction
julia> @syms x y z
(x, y, z)

```

### The expand, factor, collect, and simplify functions

A typical polynomial expression in a single variable can be written in two common ways, expanded or factored form. Using `factor` and `expand` can move between the two.

For example,

```jldoctest introduction
julia> p = x^2 + 3x + 2
 2
x  + 3⋅x + 2

julia> factor(p)
(x + 1)⋅(x + 2)


```

Or

```jldoctest introduction
julia> expand(prod((x-i) for i in 1:5))
 5       4       3        2
x  - 15⋅x  + 85⋅x  - 225⋅x  + 274⋅x - 120

```

The `factor` function factors over the rational numbers, so something like this with obvious factors is not finished:

```jldoctest introduction
julia> factor(x^2 - 2)
 2
x  - 2

```

When expressions involve one or more variables, it can be convenient to be able to manipulate them. For example,
if we define `q` by:

```jldoctest introduction
julia> q = x*y + x*y^2 + x^2*y + x
 2        2
x ⋅y + x⋅y  + x⋅y + x

```

Then we can collect the terms by the variable `x`:

```jldoctest introduction
julia> collect(q, x)
 2       ⎛ 2        ⎞
x ⋅y + x⋅⎝y  + y + 1⎠

```

or the variable `y`:

```jldoctest introduction
julia> collect(q, y)
   2         ⎛ 2    ⎞
x⋅y  + x + y⋅⎝x  + x⎠

```

These are identical expressions, though viewed differently.

A more broad-brush approach is to let `SymPy` simplify the values. In this case, the common value of `x` is factored out:

```jldoctest introduction
julia> simplify(q)
  ⎛       2        ⎞
x⋅⎝x⋅y + y  + y + 1⎠

```

The `simplify` function attempts to apply the dozens of functions related to simplification that are part of SymPy. It is also possible to apply these functions one at a time, for example `trigsimp` does trigonometric simplifications.

The SymPy tutorial illustrates that `expand` can also result in simplifications through this example:

```jldoctest introduction
julia> expand((x + 1)*(x - 2) - (x - 1)*x)
-2

```


These methods are not restricted to polynomial expressions and will
work with other expressions. For example, `factor` identifies the
following as a factorable object in terms of the variable `exp(x)`:

```jldoctest introduction
julia> factor(exp(2x) + 3exp(x) + 2)
⎛ x    ⎞ ⎛ x    ⎞
⎝ℯ  + 1⎠⋅⎝ℯ  + 2⎠

```

## Rational expressions: apart, together, cancel

When working with rational expressions, SymPy does not do much
simplification unless asked. For example this expression is not
simplified:

```jldoctest introduction
julia> r = 1/x + 1/x^2
1   1
─ + ──
x    2
    x

```

To put the terms of `r` over a common denominator, the `together` function is available:

```jldoctest introduction
julia> together(r)
x + 1
─────
   2
  x

```

The `apart` function does the reverse, creating a partial fraction decomposition from a ratio of polynomials:

```jldoctest introduction
julia> apart( (4x^3 + 21x^2 + 10x + 12) /  (x^4 + 5x^3 + 5x^2 + 4x))
 2⋅x - 1       1     3
────────── - ───── + ─
 2           x + 4   x
x  + x + 1

```

Some times SymPy will cancel factors, as here:

```jldoctest introduction
julia> top = (x-1)*(x-2)*(x-3)
(x - 3)⋅(x - 2)⋅(x - 1)

julia> bottom = (x-1)*(x-4)
(x - 4)⋅(x - 1)

julia> top/bottom
(x - 3)⋅(x - 2)
───────────────
     x - 4

```

(This might make math faculty a bit upset, but it is in line with student thinking.)

However, with expanded terms, the common factor of `(x-1)` is not cancelled:

```jldoctest introduction
julia> r = expand(top) / expand(bottom)
 3      2
x  - 6⋅x  + 11⋅x - 6
────────────────────
     2
    x  - 5⋅x + 4

```

The `cancel` function instructs SymPy to perform cancellations. It
takes rational functions and puts them in a canonical $p/q$ form with
no common (rational) factors and leading terms which are integers:

```jldoctest introduction
julia> cancel(r)
 2
x  - 5⋅x + 6
────────────
   x - 4

```

## Powers

The SymPy [tutorial](http://docs.sympy.org/dev/tutorial/simplification.html#powers) offers a thorough explanation on powers and which get simplified and under what conditions. Basically

* $x^a x^b = x^{a+b}$ is always true. However

* $x^a y^a=(xy)^a$ is only true with assumptions, such as $x,y \geq 0$ and $a$ is real, but not in general. For example, $x=y=-1$ and $a=1/2$ has $x^a \cdot y^a = i \cdot i =  -1$, where as $(xy)^a = 1$.

* $(x^a)^b = x^{ab}$ is only true with assumptions. For example $x=-1, a=2$, and $b=1/2$ gives $(x^a)^b = 1^{1/2} = 1$, whereas $x^{ab} = -1^1 = -1$.


We see that with assumptions, the following expression does simplify to $0$:

```jldoctest introduction
julia> @syms x::nonnegatve y::nonnegative  a::real
(x, y, a)

julia> simplify(x^a * y^a - (x*y)^a)
0

```

However, without assumptions this is not the case

```jldoctest introduction
julia> @syms x,y,a
(x, y, a)

julia> simplify(x^a * y^a - (x*y)^a)
 a  a        a
x ⋅y  - (x⋅y)

```

The `simplify` function calls `powsimp` to simplify powers, as above. The `powsimp` function has the keyword argument `force=true` to force simplification even if assumptions are not specified:

```jldoctest introduction
julia> powsimp(x^a * y^a - (x*y)^a, force=true)
0

```

## Trigonometric simplification

For trigonometric expressions, `simplify` will use `trigsimp` to simplify:

```jldoctest introduction
julia> @syms theta::real
(theta,)

julia> p = cos(theta)^2 + sin(theta)^2
   2         2
sin (θ) + cos (θ)

```

Calling either `simplify` or `trigsimp` will apply the Pythagorean identity:

```jldoctest introduction
julia> simplify(p)
1

```

While often forgotten,  the `trigsimp` function is, of course,  aware of the double angle formulas:

```jldoctest introduction
julia> simplify(sin(2theta) - 2sin(theta)*cos(theta))
0

```

The `expand_trig` function will expand such expressions:

```jldoctest introduction
julia> expand_trig(sin(2theta))
2⋅sin(θ)⋅cos(θ)

```


## Coefficients

Returning to polynomials, there are a few functions to find various pieces of the polynomials. First we make a general quadratic polynomial:

```jldoctest introduction
julia> @syms a,b,c,x
(a, b, c, x)

julia> p = a*x^2 + b*x + c
   2
a⋅x  + b⋅x + c

```

If given a polynomial, like `p`, there are different means to extract the coefficients:

* SymPy provides a `coeffs` method for `Poly` objects, but `p` must first be converted to one.

* SymPy provides the `coeff` method for expressions, which allows extration of a coeffiecient for a given monomial




The `ex.coeff(monom)` call will return the corresponding coefficient of the monomial:

```jldoctest introduction
julia> p.coeff(x^2) # a
a

julia> p.coeff(x)   # b
b

```

The constant can be found through substitution:

```jldoctest introduction
julia> p(x=>0)
c

```

Though one could use some trick like this to find all the coefficients:

```jldoctest introduction
julia> Sym[[p.coeff(x^i) for i in N(degree(p,gen=x)):-1:1]; p(x=>0)]
3-element Vector{Sym}:
 a
 b
 c

```

that is cumbersome, at best. SymPy has a function `coeffs`, but it is defined for polynomial types, so will fail on `p`:


```jldoctest introduction
julia> try p.coeffs() catch err "ERROR: KeyError: key `coeffs` not found" end # wrap p.coeffs() for doctest of error
"ERROR: KeyError: key `coeffs` not found"
```

Polynomials are a special class in SymPy and must be constructed. The `Poly` constructor can be used. As there is more than one free variable in `p`, we specify the variable `x` below:

```jldoctest introduction
julia> q = sympy.Poly(p, x)
Poly(a*x**2 + b*x + c, x, domain='ZZ[a,b,c]')

julia> q.coeffs()
3-element Vector{Sym}:
 a
 b
 c

```

!!! note

    The `Poly` constructor from SymPy is *not* a function, so is not exported when `SymPy` is loaded. To access it, the object must be qualified by its containing module, in this case `Poly`. Were it to be used frequently, an alias could be used, as in `const Poly=sympy.Poly` *or* the `import_from` function, as in `import_from(sympy, :Poly)`. The latter has some attempt to avoid naming collisions.


## Polynomial roots: solve, real_roots, polyroots, nroots

SymPy provides functions to find the roots of a polynomial. In
general, a polynomial with real coefficients of degree $n$ will have
$n$ roots when multiplicities and complex roots are accounted for. The
number of real roots is consequently between $0$ and $n$.

For a *univariate* polynomial expression (a single variable), the real
roots, when available, are returned by `real_roots`. For example,

```jldoctest introduction
julia> real_roots(x^2 - 2)
2-element Vector{Sym}:
 -√2
  √2

```

Unlike `factor` -- which only factors over rational factors --
`real_roots` finds the two irrational roots here. It is well known
(the
[Abel-Ruffini theorem](http://en.wikipedia.org/wiki/Abel%E2%80%93Ruffini_theorem))
that for degree 5 polynomials, or higher, it is not always possible to
express the roots in terms of radicals. However, when the roots are
rational `SymPy` can have success:


```jldoctest introduction
julia> p = (x-3)^2*(x-2)*(x-1)*x*(x+1)*(x^2 + x + 1);  string(p)
"x*(x - 3)^2*(x - 2)*(x - 1)*(x + 1)*(x^2 + x + 1)"

julia> real_roots(p)
6-element Vector{Sym}:
 -1
  0
  1
  2
  3
  3

```

!!! note "Why `string`?"
    The uses of `string(p)` above and elsewhere throughout the introduction is only for technical reasons related to doctesting and how `Documenter.jl` parses  the expected output. This usage is not idiomatic, or suggested; it  only allows the cell  to  be tested programatically for  regressions. Similarly, expected errors  are  wrapped in `try`-`catch` blocks just  for testing purposes.
	

In this example, the degree of `p` is 8, but only the 6 real roots
returned, the double root of $3$ is accounted for. The two complex
roots of `x^2 + x+ 1` are not considered by this function. The complete set
of distinct roots can be found with `solve`:

```jldoctest introduction
julia> solve(p)
7-element Vector{Sym}:
                 -1
                  0
                  1
                  2
                  3
 -1/2 - sqrt(3)*I/2
 -1/2 + sqrt(3)*I/2

```

This finds the complex roots, but does not account for the double
root. The `roots` function of SymPy does.



The output of calling `roots` will be a dictionary whose keys are the roots and values the multiplicity.

```jldoctest introduction
julia> roots(p)
Dict{Any, Any} with 7 entries:
  -1                 => 1
  3                  => 2
  1                  => 1
  0                  => 1
  2                  => 1
  -1/2 + sqrt(3)*I/2 => 1
  -1/2 - sqrt(3)*I/2 => 1

```

When exact answers are not provided, the `roots` call is contentless:

```jldoctest introduction
julia> p = x^5 - x + 1
 5
x  - x + 1

julia> roots(p)
Dict{Any, Any}()

```

Calling `solve` seems to produce very little as well:

```jldoctest introduction
julia> rts = solve(p)
5-element Vector{Sym}:
 CRootOf(x^5 - x + 1, 0)
 CRootOf(x^5 - x + 1, 1)
 CRootOf(x^5 - x + 1, 2)
 CRootOf(x^5 - x + 1, 3)
 CRootOf(x^5 - x + 1, 4)

```

But in fact, `rts` contains lots of information. We can extract numeric values quite easily with `N`:

```jldoctest introduction
julia> N.(rts)
5-element Vector{Number}:
                     -1.167303978261418684256045899854842180720560371525489039140082449275651903429536
 -0.18123244446987538 - 1.0839541013177107im
 -0.18123244446987538 + 1.0839541013177107im
   0.7648844336005847 - 0.35247154603172626im
   0.7648844336005847 + 0.35247154603172626im

```

These are numeric approximations to irrational values. For numeric
approximations to polynomial roots, the `nroots` function is also
provided. The answers are still symbolic:

```jldoctest introduction
julia> nroots(p)
5-element Vector{Sym}:
                       -1.16730397826142
 -0.181232444469875 - 1.08395410131771⋅ⅈ
 -0.181232444469875 + 1.08395410131771⋅ⅈ
 0.764884433600585 - 0.352471546031726⋅ⅈ
 0.764884433600585 + 0.352471546031726⋅ⅈ

```

## The solve function

The `solve` function is more general purpose than just finding roots of univariate polynomials. The function tries to solve for when an expression is 0, or a set of expressions are all 0.

For example, it can be used to solve when $\cos(x) = \sin(x)$:

```jldoctest introduction
julia> solve(cos(x) - sin(x))
1-element Vector{Sym}:
 pi/4

```

Though there are infinitely many correct solutions, these are within a certain range.

The
[solveset](http://docs.sympy.org/latest/modules/solvers/solveset.html)
function appears in version 1.0 of SymPy and is an intended
replacement for `solve`. Here we see it describes all solutions:

```jldoctest introduction
julia> u = solveset(cos(x) - sin(x))
⎧        5⋅π        ⎫   ⎧        π        ⎫
⎨2⋅n⋅π + ─── | n ∊ ℤ⎬ ∪ ⎨2⋅n⋅π + ─ | n ∊ ℤ⎬
⎩         4         ⎭   ⎩        4        ⎭

```

The output of `solveset` is a set, rather than a vector or
dictionary. To get the values requires some work. For *finite sets* we collect the elements
with `collect`, but first we must convert to a `Julia` `Set`:

```jldoctest introduction
julia> v = solveset(x^2 - 4)
{-2, 2}

julia> collect(Set(v...))
2-element Vector{Any}:
 -2
  2

```

This composition is done in the `elements` function:

```jldoctest introduction
julia> elements(v)
2-element Vector{Sym}:
 -2
  2

```

The `elements` function does not work for more complicated (non-finite) sets, such as `u`. For these, the `contains` method may be useful to query the underlying elements



Solving within Sympy has limits. For example, there is no symbolic solution here:

```jldoctest introduction
julia> try  solve(cos(x) - x)  catch err "error" end # wrap command for doctest of error
"error"
```

(And hence the error message generated.)

For such an equation, a numeric method would be needed, similar to the `Roots` package. For example:

```jldoctest introduction
julia> nsolve(cos(x) - x, 1)
0.7390851332151606416553120876738734040134117589007574649656806357732846548836

```

Though it can't solve everything, the `solve` function can also solve
equations of a more general type. For example, here it is used to
derive the quadratic equation:

```jldoctest introduction
julia> @syms a::real, b::real, c::real
(a, b, c)

julia> p = a*x^2 + b*x + c
   2
a⋅x  + b⋅x + c

julia> solve(p, x)
2-element Vector{Sym}:
 (-b + sqrt(-4*a*c + b^2))/(2*a)
 -(b + sqrt(-4*a*c + b^2))/(2*a)

```

The extra argument `x` is passed to `solve` so that `solve` knows
which variable to solve for.

The `solveset` function is similar:

```jldoctest introduction
julia> solveset(p, x)
⎧           _____________             _____________⎫
⎪          ╱           2             ╱           2 ⎪
⎨   b    ╲╱  -4⋅a⋅c + b       b    ╲╱  -4⋅a⋅c + b  ⎬
⎪- ─── - ────────────────, - ─── + ────────────────⎪
⎩  2⋅a         2⋅a           2⋅a         2⋅a       ⎭

```


If the `x` value is not given, `solveset` will error and  `solve` will try to find a
solution over all the free variables:

```jldoctest introduction
julia> solve(p)
1-element Vector{Dict{Any, Any}}:
 Dict(a => -(b*x + c)/x^2)
```

Systems of equations can be solved as well. We specify them within a
vector of expressions, `[ex1, ex2, ..., exn]` where a found solution
is one where all the expressions are 0. For example, to solve this
linear system: $2x + 3y = 6, 3x - 4y=12$, we have:

```jldoctest introduction
julia> @syms x::real, y::real
(x, y)

julia> exs = [2x+3y-6, 3x-4y-12]
2-element Vector{Sym}:
  2⋅x + 3⋅y - 6
 3⋅x - 4⋅y - 12

julia> d = solve(exs)
Dict{Any, Any} with 2 entries:
  y => -6/17
  x => 60/17

```



We can "check our work" by plugging into each equation. We take advantage of how the `subs` function allows us to pass in a dictionary:

```jldoctest introduction
julia> map(ex -> ex.subs(d), exs)
2-element Vector{Sym}:
 0
 0

```

The more `Julia`n way to solve a linear  equation, like this   would be as follows:

```jldoctest introduction
julia> A = Sym[2 3; 3  -4]; b = Sym[6, 12]
2-element Vector{Sym}:
  6
 12

julia> A \ b
2-element Vector{Sym}:
 60/17
 -6/17
```

(Rather than use a generic  `lu` solver through `Julia` (which  proved slow for larger  systems),  the `\` operator utilizes  `solve` to perform this  computation.)



In the previous example, the system had two equations and two
unknowns. When that is not the case, one can specify the variables to
solve for as a vector. In this example, we find a quadratic polynomial
that approximates $\cos(x)$ near $0$:

```jldoctest introduction
julia> a,b,c,h = symbols("a,b,c,h", real=true)
(a, b, c, h)

julia> p = a*x^2 + b*x + c
   2
a⋅x  + b⋅x + c

julia> fn = cos
cos (generic function with 14 methods)

julia> exs = [fn(0*h)-p(x=>0), fn(h)-p(x => h), fn(2h)-p(x => 2h)]
3-element Vector{Sym}:
                           1 - c
       -a*h^2 - b*h - c + cos(h)
 -4*a*h^2 - 2*b*h - c + cos(2*h)

julia> d = solve(exs, [a,b,c])
Dict{Any, Any} with 3 entries:
  a => -cos(h)/h^2 + cos(2*h)/(2*h^2) + 1/(2*h^2)
  b => 2*cos(h)/h - cos(2*h)/(2*h) - 3/(2*h)
  c => 1

```

Again, a dictionary is returned. The polynomial itself can be found by
substituting back in for `a`, `b`, and `c`:

```jldoctest introduction
julia> quad_approx = p.subs(d); string(quad_approx)
"x^2*(-cos(h)/h^2 + cos(2*h)/(2*h^2) + 1/(2*h^2)) + x*(2*cos(h)/h - cos(2*h)/(2*h) - 3/(2*h)) + 1"

```

Taking the "limit" as $h$ goes to 0 produces the answer $1 - x^2/2$, as  will be shown.

Finally for `solve`, we show one way to re-express the polynomial $a_2x^2 + a_1x + a_0$
as $b_2(x-c)^2 + b_1(x-c) + b_0$ using `solve` (and not, say, an
expansion theorem.)

```jldoctest introduction
julia> n = 3
3

julia> @syms x, c
(x, c)

julia> @syms as[1:3]
(Sym[as₁, as₂, as₃],)

julia> @syms bs[1:3]
(Sym[bs₁, bs₂, bs₃],)

julia> p = sum([as[i+1]*x^i for i in 0:(n-1)])
                2
2

julia> q = sum([bs[i+1]*(x-c)^i for i in 0:(n-1)])
                              2
2

julia> solve(p-q, bs)
Dict{Any, Any} with 3 entries:
  bs₃ => as₃
  bs₁ => as₁ + as₂*c + as₃*c^2
  bs₂ => as₂ + 2*as₃*c

```


### Solving using logical operators

The `solve` function does not need to just solve `ex = 0`. There are other means to specify an equation. Ideally, it would be nice to say `ex1 == ex2`, but the interpretation of `==` is not for this. Rather, `SymPy` introduces `Eq` for equality. So this expression

```jldoctest introduction
julia> solve(Eq(x, 1))
1-element Vector{Sym}:
 1

```

gives 1, as expected from solving `x == 1`.

In addition to `Eq`, there are `Lt`, `Le`, `Ge`, `Gt`. The Unicode
operators (e.g., `\leq`  and not  `\leq`)  are not aliased to these, but there are alternatives
`\ll[tab]`, `\leqq[tab]`, `\Equal[tab]`, `\geqq[tab]`, `\gg[tab]` and
`\neg[tab]` to negate.

So, the above could have been written with the following nearly identical expression, though it is entered with `\Equal[tab]`:

```jldoctest introduction
julia> solve(x ⩵ 1)
1-element Vector{Sym}:
 1

```

Here is an alternative way of asking a previous question on a pair of linear equations:

```jldoctest introduction
julia> x, y = symbols("x,y", real=true)
(x, y)

julia> exs = [2x+3y ⩵ 6, 3x-4y ⩵ 12]    ## Using \Equal[tab]
2-element Vector{Sym}:
  2⋅x + 3⋅y = 6
 3⋅x - 4⋅y = 12

julia> d = solve(exs)
Dict{Any, Any} with 2 entries:
  y => -6/17
  x => 60/17

```

Here  is  one other way  to  express  the same

```jldoctest introduction
julia> Eq.( [2x+3y,3x-4y], [6,12]) |>  solve == d
true
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

```@example plots
using SymPy, Plots
@syms x
plot(x^2 - 2, -2,2)
savefig("plot-1.svg"); nothing  # hide
```

![](plot-1.svg)

Or a parametric plot:

```@example plots
plot(sin(2x), cos(3x), 0, 4pi);
savefig("plot-2.svg"); nothing # hide
```

![](plot-2.svg)

For plotting with other plotting packages, it is generally faster to
first call `lambdify` on the expression and then generate `y` values
with the resulting `Julia` function. An example might follow this pattern:

```@example plots
ex = cos(x)^2  +  cos(x^2)
fn = lambdify(ex)
xs = range(0, stop=10, length=256)
plot(xs, fn.(xs))
savefig("plot-3.svg"); nothing #hide
```

![](plot-3.svg)

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



## Calculus

`SymPy` has many of the basic operations of calculus provided through a relatively small handful of functions.

### Limits

Limits are computed by the `limit` function which takes an expression, a variable and a value, and optionally a direction specified by either `dir="+"` or `dir="-"`.

For example, this shows Gauss was right:

```jldoctest introduction
julia> limit(sin(x)/x, x, 0)
1

```

Alternatively, the second and third arguments can be specified as a pair:

```jldoctest introduction
julia> limit(sin(x)/x, x=>0)
1

```

Limits at infinity are done by using `oo` for $\infty$:

```jldoctest introduction
julia> limit((1+1/x)^x, x => oo)
ℯ

```


This example computes what L'Hopital reportedly paid a Bernoulli for

```jldoctest introduction
julia> @syms a::positive
(a,)

julia> ex = (sqrt(2a^3*x-x^4) - a*(a^2*x)^(1//3)) / (a - (a*x^3)^(1//4));  string(ex)
"(-a^(5/3)*x^(1/3) + sqrt(2*a^3*x - x^4))/(-a^(1/4)*(x^3)^(1/4) + a)"

```

Substituting $x=a$ gives an indeterminate form:

```jldoctest introduction
julia> ex(x=>a)         # or subs(ex, x, a)
nan

```

We can see it is of the form $0/0$:

```jldoctest introduction
julia> denom(ex)(x => a), numer(ex)(x => a)
(0, 0)

```

And we get

```jldoctest introduction
julia> limit(ex, x => a)
16⋅a
────
 9

```

In a previous example, we defined `quad_approx`:

```jldoctest introduction
julia> quad_approx  |>  string
"x^2*(-cos(h)/h^2 + cos(2*h)/(2*h^2) + 1/(2*h^2)) + x*(2*cos(h)/h - cos(2*h)/(2*h) - 3/(2*h)) + 1"

```

The limit as `h` goes to $0$ gives `1 - x^2/2`, as expected:

```jldoctest introduction
julia> limit(quad_approx, h => 0)
     2
    x
1 - ──
    2

```

#### Left and right limits

The limit is defined when both the left and right limits exist and are equal. But left and right limits can exist and not be equal. The `sign` function is $1$ for positive $x$, $-1$ for negative $x$ and $0$ when $x$ is 0. It should not have a limit at $0$:

```jldoctest introduction
julia> limit(sign(x), x => 0)
1

```

Oops. Well, the left and right limits are different anyways:

```jldoctest introduction
julia> limit(sign(x), x => 0, dir="-"), limit(sign(x), x => 0, dir="+")
(-1, 1)

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

```jldoctest introduction
julia> f(x) = 1/x^(log(log(log(log(1/x)))) - 1)
f (generic function with 1 method)

```

A numeric attempt might be done along these lines:

```jldoctest introduction
julia> hs = [10.0^(-i) for i in 6:16]
11-element Vector{Float64}:
 1.0e-6
 1.0e-7
 1.0e-8
 1.0e-9
 1.0e-10
 1.0e-11
 1.0e-12
 1.0e-13
 1.0e-14
 1.0e-15
 1.0e-16

julia> ys = [f(h) for h in hs]
11-element Vector{Float64}:
 6.146316238971239e-7
 1.4298053954169988e-7
 3.4385814272678773e-8
 8.529918929292077e-9
 2.176869418153584e-9
 5.700972891527026e-10
 1.528656750900649e-10
 4.188388514215749e-11
 1.1705748589577942e-11
 3.331965462828263e-12
 9.64641441953344e-13

julia> [hs ys]
11×2 Matrix{Float64}:
 1.0e-6   6.14632e-7
 1.0e-7   1.42981e-7
 1.0e-8   3.43858e-8
 1.0e-9   8.52992e-9
 1.0e-10  2.17687e-9
 1.0e-11  5.70097e-10
 1.0e-12  1.52866e-10
 1.0e-13  4.18839e-11
 1.0e-14  1.17057e-11
 1.0e-15  3.33197e-12
 1.0e-16  9.64641e-13

```

With a values appearing to approach $0$. However, in fact these values will ultimately head  off to $\infty$:

```jldoctest introduction
julia> limit(f(x), x, 0, dir="+")
∞

```


### Derivatives

One *could* use limits to implement the definition of a derivative:

```jldoctest introduction
julia> @syms x, h
(x, h)

julia> f(x) = exp(x)*sin(x)
f (generic function with 1 method)

julia> limit((f(x+h) - f(x)) / h, h, 0)
 x           x
ℯ ⋅sin(x) + ℯ ⋅cos(x)

```

However, it would be pretty inefficient, as `SymPy` already does a great job with derivatives. The `diff` function implements this. The basic syntax is `diff(ex, x)` to find the first derivative in `x` of the expression in `ex`, or its generalization to $k$th derivatives with `diff(ex, x, k)`.

The same derivative computed above by a limit could be found with:

```jldoctest introduction
julia> diff(f(x), x)
 x           x
ℯ ⋅sin(x) + ℯ ⋅cos(x)

```

Similarly, we can compute other derivatives:

```jldoctest introduction
julia> diff(x^x, x)
 x
x ⋅(log(x) + 1)

```

Or, higher order  derivatives:

```jldoctest introduction
julia> diff(exp(-x^2), (x, 2)) |>  string
"2*(2*x^2 - 1)*exp(-x^2)"

```

As an alternate to specifying the number of derivatives, multiple variables can be passed to `diff`:

```jldoctest introduction
julia> diff(exp(-x^2), x, x, x) |>  string     # same as diff(..., (x, 3))
"4*x*(3 - 2*x^2)*exp(-x^2)"

```

This could include variables besides `x`,  as is needed with mixed partial  derivatives.


The output is a simple expression, so `diff` can be composed with
other functions, such as `solve`. For example, here we find the
critical points where the derivative is $0$ of some rational function:

```jldoctest introduction
julia> f(x) = (12x^2 - 1) / (x^3)
f (generic function with 1 method)

julia> diff(f(x), x) |> solve
2-element Vector{Sym}:
 -1/2
  1/2

```


#### Partial derivatives

The `diff` function makes finding partial derivatives as easy as specifying the variable to differentiate in. This  example computes the mixed partials of an expression in `x` and `y`:

```jldoctest introduction
julia> @syms x,y
(x, y)

julia> ex = x^2*cos(y)
 2
x ⋅cos(y)

julia> [diff(ex,v1, v2) for v1 in [x,y], v2 in [x,y]]  # also hessian(ex, (x,y))
2×2 Matrix{Sym}:
    2⋅cos(y)  -2⋅x⋅sin(y)
 -2⋅x⋅sin(y)  -x^2*cos(y)

```


#### Unevaluated derivatives

The `Derivative` constructor provides unevaluated derivatives, useful with differential equations and the output for unknown functions. Here is an example:

```jldoctest introduction
julia> ex = sympy.Derivative(exp(x*y), x, (y, 2))
   3
  ∂   ⎛ x⋅y⎞
──────⎝ℯ   ⎠
  2
∂y  ∂x

```

These expressions are evaluated with the `doit` method:

```jldoctest introduction
julia> ex.doit() |> string
"x*(x*y + 2)*exp(x*y)"

```

#### Implicit derivatives

SymPy can be used to find derivatives of implicitly defined
functions. For example, the task of finding $dy/dx$ for the equation:

$$~
y^4 - x^4 -y^2 + 2x^2 = 0
~$$

As with the mathematical solution, the key is to treat one of the variables as depending on the other. In this case, we think of $y$ locally as a function of $x$. SymPy allows us to create symbolic functions, and we will use one to substitute in for `y`.


In SymPy, symbolic functions use the class name  "Function", but in `SymPy` we use `SymFunction` to avoid a name collision with one of `Julia`'s primary types. The constructor can be used as `SymFunction(:F)`:

```jldoctest introduction
julia> F, G = SymFunction("F"), SymFunction("G")
(F, G)

```

The `@syms` macro can also more naturally be used, in place of `SymFunction`:

```jldoctest introduction
julia> @syms F(), G()
(F, G)

```


We can call these functions, but we get a function expression:

```jldoctest introduction
julia> F(x)
F(x)

```

SymPy can differentiate symbolically, again with `diff`:

```jldoctest introduction
julia> diff(F(x))
d
──(F(x))
dx

```

To get back to our problem, we have our expression:

```jldoctest introduction
julia> @syms x, y
(x, y)

julia> ex = y^4 - x^4 - y^2 + 2x^2
   4      2    4    2
- x  + 2⋅x  + y  - y

```

Now we substitute:

```jldoctest introduction
julia> ex1 = ex(y=>F(x))
   4      2    4       2
- x  + 2⋅x  + F (x) - F (x)

```

We want to differentiate "both" sides. As the right side is just $0$, there isn't anything to do here, but mentally keep track. As for the left we have:

```jldoctest introduction
julia> ex2 = diff(ex1, x)
     3            3    d                 d
- 4⋅x  + 4⋅x + 4⋅F (x)⋅──(F(x)) - 2⋅F(x)⋅──(F(x))
                       dx                dx

```

Now we collect terms and solve in terms of $F'(x)$

```jldoctest introduction
julia> ex3 = solve(ex2, F'(x))[1]
     3
  2⋅x  - 2⋅x
──────────────
   3
2⋅F (x) - F(x)

```

Finally, we substitute back into the solution for $F(x)$:

```jldoctest introduction
julia> ex4 = ex3(F(x) => y)
   3
2⋅x  - 2⋅x
──────────
    3
 2⋅y  - y

```

###### Example: A Norman Window

A classic calculus problem is to maximize the area of a
[Norman window](http://en.wiktionary.org/wiki/Norman_window) (in the
shape of a rectangle with a half circle atop) when the perimeter is
fixed to be $P \geq 0$.


Label the rectangle with $w$ and $h$ for width and height and then the
half circle has radius $r=w/2$. With this, we can see that the area is
$wh+(1/2)\pi r^2$ and the perimeter is $w + 2h + \pi r$. This gives:

```jldoctest introduction
julia> @syms w::nonnegative, h::nonnegative, P::nonnegative
(w, h, P)

julia> r = w/2
w
─
2

julia> A = w*h + 1//2 * (pi * r^2);   string(A)
"h*w + pi*w^2/8"

julia> p = w + 2h + pi*r; string(p)
"2*h + w + pi*w/2"

```

(There is a subtlety above, as m `1//2*pi*r^2` will lose exactness, as
the products will be done left to right, and `1//2*pi` will be
converted to an approximate floating point value before multiplying
`r^2`, as such we rewrite the terms. It may be easier to use `PI`
instead of `pi`.)

We want to solve for `h` from when `p=P` (our fixed value) and
substitute back into `A`. We solve `P-p==0`:

```jldoctest introduction
julia> h0 =  solve(P-p, h)[1]
P   π⋅w   w
─ - ─── - ─
2    4    2

julia> A1 = A(h => h0)
   2
π⋅w      ⎛P   π⋅w   w⎞
──── + w⋅⎜─ - ─── - ─⎟
 8       ⎝2    4    2⎠

```

Now we note this is a parabola in `w`, so any maximum will be at  an
endpoint or the vertex, provided the leading term is negative.
The leading term can be found through:

```jldoctest introduction
julia> sympy.Poly(A1, w).coeffs()
2-element Vector{Sym}:
 -1/2 - pi/8
         P/2

```

Or without using the `Poly` methods, we could do this:

```jldoctest introduction
julia> collect(expand(A1), w).coeff(w^2)
  1   π
- ─ - ─
  2   8

```

Either way, the leading coefficient, $-1/2 - \pi/8$, is negative, so
the maximum can only happen at an endpoint or the vertex of the
parabola. Now we check that when $w=0$ (the left endpoint) the area is
$0$:

```jldoctest introduction
julia> A1(w => 0)
0

```

The other endpoint is when $h=0$, or

```jldoctest introduction
julia> b = solve((P-p)(h => 0), w)[1]
 2⋅P
─────
2 + π

```

We will need to check the area at `b` and at the vertex.

To find the vertex, we can use calculus -- it will be when the derivative in `w` is $0$:

```jldoctest introduction
julia> c = solve(diff(A1, w), w)[1]
 2⋅P
─────
π + 4

```

The answer will be the larger of `A1` at `b` or `c`:

```jldoctest introduction
julia> atb = A1(w => b); string(atb)
"pi*P^2/(2*(2 + pi)^2) + 2*P*(-pi*P/(2*(2 + pi)) - P/(2 + pi) + P/2)/(2 + pi)"

julia> atc = A1(w => c);  string(atc)
"pi*P^2/(2*(pi + 4)^2) + 2*P*(-pi*P/(2*(pi + 4)) - P/(pi + 4) + P/2)/(pi + 4)"

```

A simple comparison isn't revealing:

```jldoctest introduction
julia> atc - atb |> string
"-pi*P^2/(2*(2 + pi)^2) + pi*P^2/(2*(pi + 4)^2) - 2*P*(-pi*P/(2*(2 + pi)) - P/(2 + pi) + P/2)/(2 + pi) + 2*P*(-pi*P/(2*(pi + 4)) - P/(pi + 4) + P/2)/(pi + 4)"

```

But after simplifying, we can see that this expression is positive if $P$ is:

```jldoctest introduction
julia> simplify(atc - atb) |> string
"2*P^2/(16 + pi^3 + 20*pi + 8*pi^2)"

```

With this observation, we conclude the maximum area happens at `c` with area `atc`.

### Integrals

Integration is implemented in SymPy through the `integrate` function. There are two basic calls:
`integrate(f(x), x)` will find the indefinite integral ($\int f(x) dx$) and when endpoints are specified through `integrate(f(x), (x, a, b))` the definite integral will be found ($\int_a^b f(x) dx$). The special form `integrate(ex, x, a, b)` can be used for single integrals, but the specification through a tuple is needed for multiple integrals.

Basic integrals are implemented:

```jldoctest introduction
julia> integrate(x^3, x) 
 4
x
──
4

```

Or in more generality:

```jldoctest introduction
julia> @syms n::real
(n,)

julia> ex = integrate(x^n, x)
⎧ n + 1
⎪x
⎪──────  for n ≠ -1
⎨n + 1
⎪
⎪log(x)  otherwise
⎩

```

The output here is a *piecewise function*, performing a substitution will choose a branch in this case:

```jldoctest introduction
julia> ex(n => 3)
 4
x
──
4

```

Definite integrals are just as easy. Here is Archimedes' answer:

```jldoctest introduction
julia> integrate(x^2, (x, 0, 1))
1/3

```


Tedious problems, such as those needing multiple integration-by-parts steps can be done easily:

```jldoctest introduction
julia> integrate(x^5*sin(x), x)
   5             4              3              2
- x ⋅cos(x) + 5⋅x ⋅sin(x) + 20⋅x ⋅cos(x) - 60⋅x ⋅sin(x) - 120⋅x⋅cos(x) + 120⋅s


in(x)

```

The SymPy tutorial says:

> "`integrate` uses powerful algorithms that are always improving to compute both definite and indefinite integrals, including heuristic pattern matching type algorithms, a partial implementation of the Risch algorithm, and an algorithm using Meijer G-functions that is useful for computing integrals in terms of special functions, especially definite integrals."

The tutorial gives the following example:

```jldoctest introduction
julia> ex = (x^4 + x^2*exp(x) - x^2 - 2*x*exp(x) - 2*x - exp(x))*exp(x)/((x - 1)^2*(x + 1)^2*(exp(x) + 1))
⎛ 4    2  x    2        x          x⎞  x
⎝x  + x ⋅ℯ  - x  - 2⋅x⋅ℯ  - 2⋅x - ℯ ⎠⋅ℯ
────────────────────────────────────────
              2        2 ⎛ x    ⎞
       (x - 1) ⋅(x + 1) ⋅⎝ℯ  + 1⎠

```

With indefinite integral:

```jldoctest introduction
julia> integrate(ex, x) |> string
"log(exp(x) + 1) + exp(x)/(x^2 - 1)"

```

#### Multiple integrals

The `integrate` function uses a tuple, `(var, a, b)`, to specify the limits of a definite integral. This syntax lends itself readily to multiple integration.

For example, the following computes the integral of $xy$ over the unit square:

```jldoctest introduction
julia> @syms x, y
(x, y)

julia> integrate(x*y, (y, 0, 1), (x, 0, 1))
1/4

```

The innermost terms can depend on outer ones. For example, the following integrates $x^2y$ over the upper half of the unit circle:

```jldoctest introduction
julia> integrate(x^2*y, (y, 0, sqrt(1 - x^2)), (x, -1, 1))
2/15

```


#### Unevaluated integrals

The `Integral` constructor can stage unevaluated integrals that will be evaluated by calling `doit`. It is also used when the output is unknown. This example comes from the tutorial:

```jldoctest introduction
julia> integ = sympy.Integral(sin(x^2), x)
⌠
⎮    ⎛ 2⎞
⎮ sin⎝x ⎠ dx
⌡

```

```jldoctest introduction
julia> integ.doit()  |>  string
"3*sqrt(2)*sqrt(pi)*fresnels(sqrt(2)*x/sqrt(pi))*gamma(3/4)/(8*gamma(7/4))"

```


### Taylor series

The `series` function can compute series expansions around a point to a specified order. For example,
the following command finds 4 terms of the series expansion of `exp(sin(x))` in `x` about $c=0$:

```jldoctest introduction
julia> s1 = series(exp(sin(x)), x, 0, 4); string(s1)
"1 + x + x^2/2 + O(x^4)"

```

The coefficients are from the Taylor expansion ($a_i=f^{i}(c)/i!$). The
[big "O"](http://en.wikipedia.org/wiki/Big_O_notation) term indicates
that the remainder is no bigger in  size than a constant times $x^4$,  as $x\rightarrow  0$.


Consider what happens when we multiply series of different orders:

```jldoctest introduction
julia> s2 = series(cos(exp(x)), x, 0, 6); string(s2)
"cos(1) - x*sin(1) + x^2*(-sin(1)/2 - cos(1)/2) - x^3*cos(1)/2 + x^4*(-cos(1)/4 + 5*sin(1)/24) + x^5*(-cos(1)/24 + 23*sin(1)/120) + O(x^6)"

```

```jldoctest introduction
julia> simplify(s1 * s2) |> string
"cos(1) + sqrt(2)*x*cos(pi/4 + 1) - 3*x^2*sin(1)/2 - sqrt(2)*x^3*sin(pi/4 + 1) + O(x^4)"

```

The big "O" term is $x^4$, as smaller order terms in `s2` are covered in this term. The big "O" notation is sometimes not desired, in which case the `removeO` function can be employed:

```jldoctest introduction
julia> s1.removeO() |> string
"x^2/2 + x + 1"

```



### Sums

`SymPy` can do sums, including some infinite ones. The `summation` function performs this task. For example, we have

```jldoctest introduction
julia> @syms i, n
(i, n)

julia> summation(i^2, (i, 1, n)) |> string
"n^3/3 + n^2/2 + n/6"

```

Like `Integrate` and `Derivative`, there is also a `Sum` function to stage the task until the `doit` function is called to initiate the sum.


Some famous sums can be computed:

```jldoctest introduction
julia> sn = sympy.Sum(1/i^2, (i, 1, n)); string(sn)
"Sum(i^(-2), (i, 1, n))"

julia> sn.doit()
harmonic(n, 2)

```

And from this a limit is available:

```jldoctest introduction
julia> limit(sn.doit(), n, oo) |> string
"pi^2/6"

```

This would have also been possible through `summation(1/i^2, (i, 1, oo))`.

### Vector-valued functions

Julia makes constructing a vector of symbolic objects easy:

```jldoctest introduction
julia> @syms x,y
(x, y)

julia> v = [1,2,x]
3-element Vector{Sym}:
 1
 2
 x

julia> w = [1,y,3]
3-element Vector{Sym}:
 1
 y
 3

```

The generic definitions of vector operations will work as expected with symbolic objects:

```jldoctest introduction
julia> using LinearAlgebra

julia> dot(v,w) |> string
"2*y + 3*conjugate(x) + 1"

```

Or

```jldoctest introduction
julia> cross(v,w)
3-element Vector{Sym}:
 -x⋅y + 6
    x - 3
    y - 2

```

Finding gradients can be done using a comprehension.

```jldoctest introduction
julia> ex = x^2*y - x*y^2
 2        2
x ⋅y - x⋅y

julia> Sym[diff(ex,var) for var in (x,y)]
2-element Vector{Sym}:
 2*x*y - y^2
 x^2 - 2*x*y

```


Or through broadcasting:

```jldoctest introduction
julia>  diff.(ex, (x,y))
(2*x*y - y^2, x^2 - 2*x*y)
```

The mixed partials is similarly done by passing two variables to differentiate in to `diff`, as illustrated previously:

```jldoctest introduction
julia> Sym[diff(ex, v1, v2) for v1 in (x,y), v2 in (x,y)]
2×2 Matrix{Sym}:
       2⋅y  2⋅(x - y)
 2⋅(x - y)       -2⋅x

```

For this task, SymPy provides the `hessian` method:

```jldoctest introduction
julia> hessian(ex, (x,y))
2×2 Matrix{Sym}:
       2⋅y  2⋅x - 2⋅y
 2⋅x - 2⋅y       -2⋅x

```

## Matrices

`Julia` has excellent infrastructure to work with generic matrices,
such as `Matrix{Sym}` objects (matrices with symbolic entries). As
well, SymPy has a class for matrices. `SymPy`, through `PyCall`, automatically maps mutable SymPy matrices into `Julia`n matrices of type `Array{Sym}`.



Constructing matrices with symbolic entries follows `Julia`'s conventions:
	
```jldoctest introduction
julia> @syms x,y
(x, y)

julia> M = [1 x; x 1]
2×2 Matrix{Sym}:
 1  x
 x  1

```

Construction of symbolic matrices can *also* be done through the `Matrix` constructor, which must be qualified. It is passed a vector or row vectors but any symbolic values *must* be converted into `PyObject`s:

```jldoctest introduction
julia> import PyCall: PyObject

julia> A = sympy.Matrix([[1,PyObject(x)], [PyObject(y), 2]])
2×2 Matrix{Sym}:
 1  x
 y  2
```

(otherwise, an entry like `[1,x]` will be mapped to a `Vector{Sym}` prior to passing to `sympy.Matrix` and the processing get's done differently, and not as desired.)

Alternatively, using tuples will avoid  the behind-the-scenes conversion:

```jldoctest introduction
julia> A = sympy.Matrix( ((1,x),  (y,2)) )
2×2 Matrix{Sym}:
 1  x
 y  2
```

Either is useful if copying SymPy examples, but otherwise unneccesary, these are immediately mapped into `Julia` arrays by `PyCall`.
**Unless** an immutable array is desired, and then the `sympy.ImmutableMatrix` constructor is used.



```jldoctest introduction
julia> diagm(0=>ones(Sym, 5))
5×5 Matrix{Sym}:
 1  0  0  0  0
 0  1  0  0  0
 0  0  1  0  0
 0  0  0  1  0
 0  0  0  0  1

julia> M^2
2×2 Matrix{Sym}:
 x^2 + 1      2⋅x
     2⋅x  x^2 + 1

julia> det(M)
     2
1 - x
```

Similarly,

```jldoctest introduction
julia> A^2
2×2 Matrix{Sym}:
 x⋅y + 1      3⋅x
     3⋅y  x⋅y + 4

```

We can call `Julia`'s generic matrix functions in the usual manner, e.g:

```jldoctest introduction
julia> det(A)
-x⋅y + 2

```

We can also call SymPy's matrix methods using the dot-call syntax:


```jldoctest introduction
julia> A.det()
-x⋅y + 2

```

(Actually, `det(A)` avoids the generic `Julia` implementation, but a determinant  can be found  using  `Julia`'s  generic `lu` function, as long as no pivoting is specified.)


Occasionally, the SymPy method has more content:

```jldoctest introduction
julia> eigvecs(M)
2×2 Matrix{Sym}:
 -1  1
  1  1

```

As compared to SymPy's `eigenvects` which yields:

```jldoctest introduction
julia> A.eigenvects()
2-element Vector{Tuple{Sym, Int64, Vector{Matrix{Sym}}}}:
 (3/2 - sqrt(4*x*y + 1)/2, 1, [[-2*x/(sqrt(4*x*y + 1) - 1); 1]])
 (sqrt(4*x*y + 1)/2 + 3/2, 1, [[2*x/(sqrt(4*x*y + 1) + 1); 1]])

```

(This is a bit misleading, as the generic `eigvecs` fails on `M`, so the value is basically just repackaged from `A.eigenvects()`.)

This example from the tutorial shows the `nullspace` function:

```jldoctest introduction
julia> A = Sym[1 2 3 0 0; 4 10 0 0 1]
2×5 Matrix{Sym}:
 1   2  3  0  0
 4  10  0  0  1

julia> vs = A.nullspace()
3-element Vector{Matrix{Sym}}:
 [-15; 6; … ; 0; 0]
 [0; 0; … ; 1; 0]
 [1; -1/2; … ; 0; 1]

```

And this shows that they are indeed in the null space of `M`:

```jldoctest introduction
julia> [A*vs[i] for i in 1:3]
3-element Vector{Matrix{Sym}}:
 [0; 0]
 [0; 0]
 [0; 0]

```

Symbolic expressions can be included in the matrices:

```jldoctest introduction
julia> A = [1 x; x 1]
2×2 Matrix{Sym}:
 1  x
 x  1

julia> P, D = A.diagonalize()  # M = PDP^-1
(Sym[-1 1; 1 1], Sym[1 - x 0; 0 x + 1])

julia> A - P*D*inv(P)
2×2 Matrix{Sym}:
 0  0
 0  0

```


## Differential equations

SymPy has facilities for solving ordinary differential
[equations](http://docs.sympy.org/latest/modules/solvers/ode.html). The
key is to create a symbolic function expression using
`SymFunction`. Again, this may be done through:

```jldoctest introduction
julia> @syms F()
(F,)

```

With this, we can  construct a  differential equation. Following the SymPy tutorial, we solve $f''(x) - 2f'(x) + f(x) = \sin(x)$:

```jldoctest introduction
julia> diffeq = Eq(diff(F(x), x, 2) - 2*diff(F(x)) + F(x), sin(x)); string(diffeq)
"Eq(F(x) - 2*Derivative(F(x), x) + Derivative(F(x), (x, 2)), sin(x))"

```


With this, we just need the `dsolve` function. This is called as `dsolve(eq)` or `dsolve(eq, F(x))`:

```jldoctest introduction
julia> ex = dsolve(diffeq, F(x)); string(ex)
"Eq(F(x), (C1 + C2*x)*exp(x) + cos(x)/2)"

```


The `dsolve` function in SymPy has an extensive list of named
arguments to control the underlying algorithm. These can be passed
through with the appropriate keyword arguments. (To use SymPy's `ics` argument, the `sympy.dsolve` method must be called directly.)

The `SymFunction` objects have the `'` method defined to
find a derivative. 
The above could also have been expressed  more familiarly through:

```jldoctest introduction
julia> diffeq = F''(x) - 2F'(x) + F(x) - sin(x); string(diffeq)
"F(x) - sin(x) - 2*Derivative(F(x), x) + Derivative(F(x), (x, 2))"

julia> sympy.dsolve(diffeq, F(x)) |> string
"Eq(F(x), (C1 + C2*x)*exp(x) + cos(x)/2)"

```

!!! note
    Using the `adjoint` operator for differentiation as a convenience is nice, but it has potential issues, as it preferences treating `SymFunction` objects as functions of a single variable and makes their  usages within  arrays possible problematic, as the  recursive `adjoint` function will not perform the generic  task. 



This solution has two constants, $C_1$ and $C_2$, that would be found from initial conditions. Say we know $F(0)=0$ and $F'(0)=1$, can we find the constants? To work with the returned expression, it is most convenient to get just the right hand side. The `rhs` method will return the right-hand side of a relation:

```jldoctest introduction
julia> ex1 = ex.rhs(); string(ex1)
"(C1 + C2*x)*exp(x) + cos(x)/2"

```

(The
[args](http://docs.sympy.org/dev/modules/core.html#sympy.core.basic.Basic.args)
function also can be used to break up the expression into parts.)

With this, we can solve for `C1` through substituting in $0$ for $x$:

```jldoctest introduction
julia> C1 = first(free_symbols(ex1))
C₁

julia> solve(ex1(x => 0), C1)
1-element Vector{Sym}:
 -1/2

```

We see that $C1=-1/2$, which we substitute in:

```jldoctest introduction
julia> ex2 = ex1(C1 => -Sym(1//2)); string(ex2)
"(C2*x - 1/2)*exp(x) + cos(x)/2"

```

We know that $F'(0)=1$ now, so we solve for `C2` through

```jldoctest introduction
julia> C2 = free_symbols(ex1)[2]
C₂

julia> solve( diff(ex2, x)(x => 0) - 1, C2 )
1-element Vector{Sym}:
 3/2
```

This gives `C2=3/2`. Again we substitute in to get our answer:

```jldoctest introduction
julia> ex3 = ex2(Sym("C2") => 3//2); string(ex3)
"(3*x/2 - 1/2)*exp(x) + cos(x)/2"

```



###### Example

We do one more example, this one borrowed from [here](http://nbviewer.ipython.org/github/garth-wells/IA-maths-Ipython/blob/master/notebooks/Lecture1.ipynb).

> Find the variation of speed with time of a parachutist subject to a drag force of $k\cdot v^2$.

The equation is

$$~
\frac{m}{k} \frac{dv}{dt} = \alpha^2 - v^2.
~$$

We proceed through:

```jldoctest introduction
julia> @syms t, m, k, alpha=>"α", v()
(t, m, k, α, v)

julia> ex = Eq( (m/k)*v'(t), alpha^2 - v(t)^2 )
  d
m⋅──(v(t))
  dt          2    2
────────── = α  - v (t)
    k

```

We can "classify" this ODE with the method `classify_ode` function.

```jldoctest introduction
julia> sympy.classify_ode(ex)
("separable", "1st_power_series", "lie_group", "separable_Integral")

```

It is linear, but not solvable. Proceeding with `dsolve` gives:

```jldoctest introduction
julia> dsolve(ex, v(t)) |> string
"Eq(v(t), -α/tanh(log(exp(k*α*(C1 - 2*t)))/(2*m)))"

```


### Initial Value Problems

Solving an initial value problem can be a bit tedious with `SymPy`.
The first example shows the steps. This is because the `ics` argument
for `sympy.dsolve` only works for a few types of equations. These do not
include, by default, the familiar "book" examples, such as $y'(x) =
a\cdot y(x)$.

To work around this, `SymPy.jl` extends the function `sympy.dsolve` to allow
a specification of the initial conditions when solving.  Each initial condition is specified with 3-tuple. For example, `v(t0)=v0` is specified with `(v, t0, v0)`.  The conditions on the values functions may use `v`, `v'`, ...
To illustrate, we follow an example from
[Wolfram](https://reference.wolfram.com/language/tutorial/DSolveLinearBVPs.html).

```jldoctest introduction
julia> @syms y(), a, x
(y, a, x)

julia> eqn = y'(x) - 3*x*y(x) - 1; string(eqn)
"-3*x*y(x) + Derivative(y(x), x) - 1"

```



We solve the initial value problem with $y(0) = 4$ as follows:

```jldoctest introduction
julia> x0, y0 = 0, 4
(0, 4)

julia> out = dsolve(eqn, x, ics = (y, x0, y0)); string(out)
"Eq(y(x), (sqrt(6)*sqrt(pi)*erf(sqrt(6)*x/2)/6 + 4)*exp(3*x^2/2))"

```

Verifying this requires combining some operations:

```jldoctest introduction
julia> u = out.rhs(); string(u)
"(sqrt(6)*sqrt(pi)*erf(sqrt(6)*x/2)/6 + 4)*exp(3*x^2/2)"

julia> diff(u, x) - 3*x*u - 1
0

```

To solve with a general initial condition is similar:

```jldoctest introduction
julia> x0, y0 = 0, a
(0, a)

julia> out = dsolve(eqn, x, ics=(y, x0, y0)); string(out)
"Eq(y(x), (a + sqrt(6)*sqrt(pi)*erf(sqrt(6)*x/2)/6)*exp(3*x^2/2))"

```


To plot this over a range of values for `a` we would have:

```@example plots
y = SymFunction("y"); nothing #hide
a, x = symbols("a,x");  nothing #hide
eqn = y'(x) - 3*x*y(x) - 1; nothing #hide
x0, y0 = 0, a; nothing #hide
out = dsolve(eqn, x, ics = (y, x0, y0)); nothing #hide

as = -2:0.6:2
fn = lambdify(subs(out.rhs(), a=>first(as)))
xs = range(-1.8, 1.8, length=500)
p = plot(xs, fn.(xs), legend=false, ylim=(-4,4))
for aᵢ in as[2:end]
    fn  = lambdify(subs(out.rhs(), a=>aᵢ))
    plot!(p, xs, fn.(xs))
end
p
savefig("plot-9.svg"); nothing #hide
```

![](plot-9.svg)

The comment from the example is "This plots several integral curves of the equation for different values of $a$. The plot shows that the solutions have an inflection point if the parameter  lies between $-1$ and $1$ , while a global maximum or minimum arises for other values of $a$."


##### Example

We continue with another example from the Wolfram documentation, that
of solving $y'' + 5y' + 6y=0$ with values prescribed for both $y$ and
$y'$ at $x_0=0$.

```jldoctest introduction
julia> @syms y(), x
(y, x)

julia> eqn = y''(x) + 5y'(x) + 6y(x);  string(eqn)
"6*y(x) + 5*Derivative(y(x), x) + Derivative(y(x), (x, 2))"

```

To solve with $y(0) = 1$ and $y'(0) = 1$ we have:

```jldoctest introduction
julia> out = dsolve(eqn, x, ics=((y, 0, 1), (y', 0, 1))); string(out)
"Eq(y(x), (4 - 3*exp(-x))*exp(-2*x))"

```

(That is we combine *all* initial conditions with a tuple.)

To make a plot, we only need the right-hand-side of the answer:

```@example plots
y = SymFunction("y"); nothing #hide
x = symbols("x"); nothing #hide
eqn = y''(x) + 5y'(x) + 6y(x); nothing  #hide
out = dsolve(eqn, x, ics=((y, 0, 1), (y', 0, 1))); nothing #hide
plot(out.rhs(), -1/3, 2)
savefig("plot-10.svg"); nothing  # hide
```

![](plot-10.svg)

##### Example

Boundary value problems can be solved for, as well, through a similar
syntax. Continuing with examples from the
[Wolfram](https://reference.wolfram.com/language/tutorial/DSolveLinearBVPs.html)
page, we solve $y''(x) +y(x) = e^x$ over $[0,1]$ with conditions
$y(0)=1$, $y(1) = 1/2$:

```jldoctest introduction
julia> eqn = y''(x) + y(x) - exp(x); string(eqn)
"y(x) - exp(x) + Derivative(y(x), (x, 2))"

julia> dsolve(eqn, x, ics=((y, 0, 1), (y, 1, 1//2))) |> string
"Eq(y(x), exp(x)/2 + (-E - cos(1) + 1)*sin(x)/(2*sin(1)) + cos(x)/2)"

```
