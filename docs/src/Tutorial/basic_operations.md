# Basic Operations

[From](https://docs.sympy.org/latest/tutorial/basic_operations.html)


Here we discuss some of the most basic operations needed for expression
manipulation in SymPy.  Some more advanced operations will be discussed later
in the :ref:`advanced expression manipulation <tutorial-manipulation>` section.

```verbatim
    >>> from sympy import *
    >>> x, y, z = symbols("x y z")
```

##### In `Julia`:

```
using SymPy
x, y, z = symbols("x y z")
```

* We didn't replicate `from sympy import *`, though this is mostly
  done through the command `import_from(sympy)`.  By default, `SymPy`
  only makes available a priviledged collection of the functions
  available through the `sympy` object. The `import_from` imports most
  all of the rest.

* If a function is not imported, it may be referenced through qualification, asin `sympy.expand_trig`, as will be seen in the following.

----

## Substitution


One of the most common things you might want to do with a mathematical
expression is substitution.  Substitution replaces all instances of something
in an expression with something else.  It is done using the `subs` method.
For example

```verbatim
    >>> expr = cos(x) + 1
    >>> expr.subs(x, y)
    cos(y) + 1
```

##### In `Julia`:

```
expr = cos(x) + 1
expr.subs(x, y)
```

Julia also allows "call" notation using a pairs to indicate the substitution:

```
expr(x => y)
```

----

Substitution is usually done for one of two reasons:

1. Evaluating an expression at a point. For example, if our expression is
   `cos(x) + 1` and we want to evaluate it at the point `x = 0`, so that
   we get `cos(0) + 1`, which is 2.

```verbatim
   >>> expr.subs(x, 0)
   2
```

##### In `Julia`:

```
expr(x => 0)
```

----

2. Replacing a subexpression with another subexpression.  There are two
   reasons we might want to do this.  The first is if we are trying to build
   an expression that has some symmetry, such as `x^{x^{x^x}}`.  To build
   this, we might start with `x**y`, and replace `y` with `x**y`.  We
   would then get `x**(x**y)`.  If we replaced `y` in this new expression
   with `x**x`, we would get `x**(x**(x**x))`, the desired expression.

```verbatim
   >>> expr = x**y
   >>> expr
   x**y
   >>> expr = expr.subs(y, x**y)
   >>> expr
   x**(x**y)
   >>> expr = expr.subs(y, x**x)
   >>> expr
   x**(x**(x**x))
```

##### In `Julia`:

```
expr = x^y
expr
```

```
expr = expr(y => x^y)
expr
```

```
expr = expr(y => x^x)
expr
```

----

The second is if we want to perform a very controlled simplification, or
   perhaps a simplification that SymPy is otherwise unable to do.  For
   example, say we have `\sin(2x) + \cos(2x)`, and we want to replace
   `\sin(2x)` with `2\sin(x)\cos(x)`.  As we will learn later, the function
   `expand_trig` does this.  However, this function will also expand
   `\cos(2x)`, which we may not want.  While there are ways to perform such
   precise simplification, and we will learn some of them in the
   :ref:`advanced expression manipulation <tutorial-manipulation>` section, an
   easy way is to just replace `\sin(2x)` with `2\sin(x)\cos(x)`.

```verbatim
   >>> expr = sin(2*x) + cos(2*x)
   >>> expand_trig(expr)
   2*sin(x)*cos(x) + 2*cos(x)**2 - 1
   >>> expr.subs(sin(2*x), 2*sin(x)*cos(x))
   2*sin(x)*cos(x) + cos(2*x)
```

##### In `Julia`:

* `expand_trig` is not exported, so we qualify it:

```
expr = sin(2*x) + cos(2*x)
sympy.expand_trig(expr)
```

```
expr(sin(2*x) => 2*sin(x)*cos(x))
```

----

There are two important things to note about `subs`.  First, it returns a new expression.  SymPy objects are immutable.  That means that `subs` does not modify it in-place.  For example

```verbatim
   >>> expr = cos(x)
   >>> expr.subs(x, 0)
   1
   >>> expr
   cos(x)
   >>> x
   x
```

##### In `Julia`:

```
expr = cos(x)
expr(x => 0)
```

```
expr
```

```
x
```

----

!!! note "Quick Tip"

   SymPy expressions are immutable.  No function will change them in-place.


Here, we see that performing `expr.subs(x, 0)` leaves `expr` unchanged.
In fact, since SymPy expressions are immutable, no function will change them
in-place.  All functions will return new expressions.

To perform multiple substitutions at once, pass a list of `(old, new)` pairs
to `subs`.

```verbatim
    >>> expr = x**3 + 4*x*y - z
    >>> expr.subs([(x, 2), (y, 4), (z, 0)])
    40
```

##### In `Julia`:

```
expr = x^3 + 4*x*y - z
expr.subs([(x, 2), (y, 4), (z, 0)])
```

Or, using pairs:

```
expr(x => 2, y=>4, z => 0)
```

----

It is often useful to combine this with a list comprehension to do a large set
of similar replacements all at once.  For example, say we had `x^4 - 4x^3 + 4x^2 -
2x + 3` and we wanted to replace all instances of `x` that have an even power
with `y`, to get `y^4 - 4x^3 + 4y^2 - 2x + 3`.

```verbatim
    >>> expr = x**4 - 4*x**3 + 4*x**2 - 2*x + 3
    >>> replacements = [(x**i, y**i) for i in range(5) if i % 2 == 0]
    >>> expr.subs(replacements)
    -4*x**3 - 2*x + y**4 + 4*y**2 + 3
```

##### In `Julia`:

```
expr = x^4 - 4*x^3 + 4*x^2 - 2*x + 3
replacements = [(x^i, y^i) for i in 1:5 if iseven(i)]
expr.subs(replacements)
```

----

## Converting Strings to SymPy Expressions


The `sympify` function (that's `sympify`, not to be confused with
`simplify`) can be used to convert strings into SymPy expressions.

For example

```verbatim
    >>> str_expr = "x**2 + 3*x - 1/2"
    >>> expr = sympify(str_expr)
    >>> expr
    x**2 + 3*x - 1/2
    >>> expr.subs(x, 2)
    19/2
```

##### In `Julia`:

As `sympify` is not passed a symbolic value, it is qualified:

```
str_expr = "x^2 + 3*x - 1/2"
expr = sympy.sympify(str_expr)
expr
```

```
expr.subs(x, 2)
```

----

!!! note "Alert:"

    `sympify` uses `eval`.  Don't use it on unsanitized input.

## `evalf`


To evaluate a numerical expression into a floating point number, use
`evalf`.

```verbatim
    >>> expr = sqrt(8)
    >>> expr.evalf()
    2.82842712474619
```

##### In `Julia`:

* We must use a symbolic value for `8`:

```
expr = sqrt(Sym(8))
expr.evalf()
```

!!! note "N"

    More importantly, `SymPy.jl` treats `N` differently from
    `evalf`. `N` is used to convert a SymPy numeric (or Boolean) value
    to a `Julia`n counterpart. The main difference between `N(x)` and
    `convert(T, x)`, is that rather than specify the `Julia` type as
    `T`, `N` works to guess the appropriate type for the `SymPy`
    object.

```
N(sqrt(8))   # brings back as BigFloat
```

```
N(sqrt(9))   # an Int
```


----

SymPy can evaluate floating point expressions to arbitrary precision.  By
default, 15 digits of precision are used, but you can pass any number as the
argument to `evalf`.  Let's compute the first 100 digits of `\pi`.

```verbatim
    >>> pi.evalf(100)
3.141592653589793238462643383279502884197169399375105820974944592307816406286208998628034825342117068
```

##### In `Julia`:

```
PI.evalf(100)
```

----

To numerically evaluate an expression with a Symbol at a point, we might use
`subs` followed by `evalf`, but it is more efficient and numerically
stable to pass the substitution to `evalf` using the `subs` flag, which
takes a dictionary of `Symbol: point` pairs.

```verbatim
    >>> expr = cos(2*x)
    >>> expr.evalf(subs={x: 2.4})
    0.0874989834394464
```

##### In `Julia`:

A Dict can be used:

```
expr = cos(2*x)
expr.evalf(subs=Dict(x => 2.4))
```

----

Sometimes there are roundoff errors smaller than the desired precision that
remain after an expression is evaluated. Such numbers can be removed at the
user's discretion by setting the `chop` flag to True.

```verbatim
    >>> one = cos(1)**2 + sin(1)**2
    >>> (one - 1).evalf()
    -0.e-124
    >>> (one - 1).evalf(chop=True)
    0
```

##### In `Julia`:

* we need to use symbolic values for `1`:

```
_one = cos(Sym(1))^2 + sin(Sym(1))^2
(_one - 1).evalf()
```

```
(_one - 1).evalf(chop=true)
```

----

## `N` with `Julia`

The `N` function is used to convert a symbolic number or boolean into a `Julia` counterpart.

```
two = Sym(2)
a,b,c,d = two, sqrt(two), two^20, two^100
N.((a,b,c,d))
```


## `lambdify`


`subs` and `evalf` are good if you want to do simple evaluation, but if
you intend to evaluate an expression at many points, there are more efficient
ways.  For example, if you wanted to evaluate an expression at a thousand
points, using SymPy would be far slower than it needs to be, especially if you
only care about machine precision.  Instead, you should use libraries like
`NumPy <http://www.numpy.org/>`_ and `SciPy <http://www.scipy.org/>`_.

The easiest way to convert a SymPy expression to an expression that can be
numerically evaluated is to use the `lambdify` function.  `lambdify` acts
like a `lambda` function, except it converts the SymPy names to the names of
the given numerical library, usually NumPy.  For example

```verbatim
    >>> import numpy # doctest:+SKIP
    >>> a = numpy.arange(10) # doctest:+SKIP
    >>> expr = sin(x)
    >>> f = lambdify(x, expr, "numpy") # doctest:+SKIP
    >>> f(a) # doctest:+SKIP
    [ 0.          0.84147098  0.90929743  0.14112001 -0.7568025  -0.95892427
     -0.2794155   0.6569866   0.98935825  0.41211849]
```


```
alert("""
`lambdify` uses `eval`.  Don't use it on unsanitized input.
""")
```

##### In `Julia`:

* `lambdify` is defined seperately and with a different argument order: `lambdify(ex, vars=free_symbols(ex))`.

```
a = 0:10
@vars x
expr = sin(x)
fn = lambdify(expr)
fn.(a)
```

----

You can use other libraries than NumPy. For example, to use the standard
library math module, use `"math"`.

```verbatim
    >>> f = lambdify(x, expr, "math")
    >>> f(0.1)
    0.0998334166468
```

##### In `Julia`:

* this doesn't apply, so is not implemented.


----

To use lambdify with numerical libraries that it does not know about, pass a
dictionary of `sympy_name:numerical_function` pairs.  For example

```verbatim
    >>> def mysin(x):
    ...     """
    ...     My sine. Note that this is only accurate for small x.
    ...     """
    ...     return x
    >>> f = lambdify(x, expr, {"sin":mysin})
    >>> f(0.1)
    0.1
```

##### In `Julia`:

* The `fns` dictionary coud be used to do this, though due to the call of `eval`, we must do this in the proper module:

```
mysin(x) = cos(x)
ex = sin(x)
body = SymPy.walk_expression(ex, fns=Dict("sin" => :mysin))
syms = (:x,)
fn = eval(Expr(:function, Expr(:call, gensym(), syms...), body))
fn(0)
```

----

!!! note "TODO"

    Write an advanced numerics section


----

[return to index](./index.html)
