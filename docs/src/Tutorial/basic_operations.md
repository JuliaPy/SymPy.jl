# Basic Operations

[From](https://docs.sympy.org/latest/tutorial/basic_operations.html)


Here we discuss some of the most basic operations needed for expression
manipulation in SymPy.  Some more advanced operations will be discussed later
in the :ref:`advanced expression manipulation <tutorial-manipulation>` section.

```python
    >>> from sympy import *
    >>> x, y, z = symbols("x y z")
```

##### In `Julia`:

```@setup basicoperations
using SymPy
sympy.init_printing(use_unicode=True)
```

```jldoctest basicoperations
julia> using SymPy

julia> x, y, z = symbols("x y z")
(x, y, z)
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

```python
    >>> expr = cos(x) + 1
    >>> expr.subs(x, y)
    cos(y) + 1
```

##### In `Julia`:

```jldoctest basicoperations
julia> expr = cos(x) + 1
cos(x) + 1

julia> expr.subs(x, y)
cos(y) + 1
```

Julia also allows "call" notation using a pairs to indicate the substitution:

```jldoctest basicoperations
julia> expr(x => y)
cos(y) + 1
```

----

Substitution is usually done for one of two reasons:

1. Evaluating an expression at a point. For example, if our expression is
   `cos(x) + 1` and we want to evaluate it at the point `x = 0`, so that
   we get `cos(0) + 1`, which is 2.

```python
   >>> expr.subs(x, 0)
   2
```

##### In `Julia`:

```jldoctest basicoperations
julia> expr(x => 0)
2
```

----

2. Replacing a subexpression with another subexpression.  There are two
   reasons we might want to do this.  The first is if we are trying to build
   an expression that has some symmetry, such as `x^{x^{x^x}}`.  To build
   this, we might start with `x**y`, and replace `y` with `x**y`.  We
   would then get `x**(x**y)`.  If we replaced `y` in this new expression
   with `x**x`, we would get `x**(x**(x**x))`, the desired expression.

```python
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

```jldoctest basicoperations
julia> expr = x^y
 y
x 

julia> expr = expr(y => x^y)
 ⎛ y⎞
 ⎝x ⎠
x    

julia> expr = expr(y => x^x)
 ⎛ ⎛ x⎞⎞
 ⎜ ⎝x ⎠⎟
 ⎝x    ⎠
x       

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

```python
   >>> expr = sin(2*x) + cos(2*x)
   >>> expand_trig(expr)
   2*sin(x)*cos(x) + 2*cos(x)**2 - 1
   >>> expr.subs(sin(2*x), 2*sin(x)*cos(x))
   2*sin(x)*cos(x) + cos(2*x)
```

##### In `Julia`:

* `expand_trig` is not exported, so we qualify it:

```jldoctest basicoperations
julia> expr = sin(2*x) + cos(2*x)
sin(2⋅x) + cos(2⋅x)

julia> sympy.expand_trig(expr) |> string
"2*sin(x)*cos(x) + 2*cos(x)^2 - 1"

julia> expr(sin(2*x) => 2*sin(x)*cos(x))
2⋅sin(x)⋅cos(x) + cos(2⋅x)
```

----

There are two important things to note about `subs`.  First, it returns a new expression.  SymPy objects are immutable.  That means that `subs` does not modify it in-place.  For example

```python
   >>> expr = cos(x)
   >>> expr.subs(x, 0)
   1
   >>> expr
   cos(x)
   >>> x
   x
```

##### In `Julia`:

```jldoctest basicoperations
julia> expr = cos(x)
cos(x)

julia> expr(x => 0)
1

julia> expr
cos(x)

julia> x
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

```python
    >>> expr = x**3 + 4*x*y - z
    >>> expr.subs([(x, 2), (y, 4), (z, 0)])
    40
```

##### In `Julia`:

```jldoctest basicoperations
julia> expr = x^3 + 4*x*y - z;  string(expr)
"x^3 + 4*x*y - z"

julia> expr.subs([(x, 2), (y, 4), (z, 0)])
40
```

Or, using pairs:

```jldoctest basicoperations
julia> expr(x=>2, y=>4, z=>0)
40
```

----

It is often useful to combine this with a list comprehension to do a large set
of similar replacements all at once.  For example, say we had `x^4 - 4x^3 + 4x^2 -
2x + 3` and we wanted to replace all instances of `x` that have an even power
with `y`, to get `y^4 - 4x^3 + 4y^2 - 2x + 3`.

```python
    >>> expr = x**4 - 4*x**3 + 4*x**2 - 2*x + 3
    >>> replacements = [(x**i, y**i) for i in range(5) if i % 2 == 0]
    >>> expr.subs(replacements)
    -4*x**3 - 2*x + y**4 + 4*y**2 + 3
```

##### In `Julia`:

```jldoctest basicoperations
julia> expr = x^4 - 4*x^3 + 4*x^2 - 2*x + 3
 4      3      2
x  - 4⋅x  + 4⋅x  - 2⋅x + 3

julia> replacements = [(x^i, y^i) for i in 1:5 if iseven(i)]
2-element Array{Tuple{Sym,Sym},1}:
 (x^2, y^2)
 (x^4, y^4)

julia> expr.subs(replacements)
     3          4      2
- 4⋅x  - 2⋅x + y  + 4⋅y  + 3
```

----

## Converting Strings to SymPy Expressions


The `sympify` function (that's `sympify`, not to be confused with
`simplify`) can be used to convert strings into SymPy expressions.

For example

```python
    >>> str_expr = "x**2 + 3*x - 1/2"
    >>> expr = sympify(str_expr)
    >>> expr
    x**2 + 3*x - 1/2
    >>> expr.subs(x, 2)
    19/2
```

##### In `Julia`:

As `sympify` is not passed a symbolic value, it is qualified:

```jldoctest basicoperations
julia> str_expr = "x^2 + 3*x - 1/2"
"x^2 + 3*x - 1/2"

julia> expr = sympy.sympify(str_expr)
 2         1
x  + 3⋅x - ─
           2
julia> expr.subs(x, 2)
19/2
```

----

!!! note "Alert:"
    `sympify` uses `eval`.  Don't use it on unsanitized input.

## `evalf`


To evaluate a numerical expression into a floating point number, use
`evalf`.

```python
    >>> expr = sqrt(8)
    >>> expr.evalf()
    2.82842712474619
```

##### In `Julia`:

* We must use a symbolic value for `8`:

```jldoctest basicoperations
julia> expr = sqrt(Sym(8))
2⋅√2

julia> expr.evalf()
2.82842712474619
```

!!! note "N is different in SymPy.jl"

    More importantly, `SymPy.jl` treats `N` differently from
    `evalf`. `N` is used to convert a SymPy numeric (or Boolean) value
    to a `Julia`n counterpart. The main difference between `N(x)` and
    `convert(T, x)`, is that rather than specify the `Julia` type as
    `T`, `N` works to guess the appropriate type for the `SymPy`
    object.

```jldoctest basicoperations
julia> N(sqrt(8))   # brings back as BigFloat
2.8284271247461903
```

```jldoctest basicoperations
julia> N(sqrt(9))   # an Int
3.0
```


----

SymPy can evaluate floating point expressions to arbitrary precision.  By
default, 15 digits of precision are used, but you can pass any number as the
argument to `evalf`.  Let's compute the first 100 digits of `\pi`.

```python
    >>> pi.evalf(100)
3.141592653589793238462643383279502884197169399375105820974944592307816406286208998628034825342117068
```

##### In `Julia`:

```jldoctest basicoperations
julia> PI.evalf(100)
3.141592653589793238462643383279502884197169399375105820974944592307816406286208998628034825342117068
```

----

To numerically evaluate an expression with a Symbol at a point, we might use
`subs` followed by `evalf`, but it is more efficient and numerically
stable to pass the substitution to `evalf` using the `subs` flag, which
takes a dictionary of `Symbol: point` pairs.

```python
    >>> expr = cos(2*x)
    >>> expr.evalf(subs={x: 2.4})
    0.0874989834394464
```

##### In `Julia`:

A Dict can be used:

```jldoctest basicoperations
julia> expr = cos(2*x)
cos(2⋅x)

julia> expr.evalf(subs=Dict(x => 2.4))
0.0874989834394464
```

----

Sometimes there are roundoff errors smaller than the desired precision that
remain after an expression is evaluated. Such numbers can be removed at the
user's discretion by setting the `chop` flag to True.

```python
    >>> one = cos(1)**2 + sin(1)**2
    >>> (one - 1).evalf()
    -0.e-124
    >>> (one - 1).evalf(chop=True)
    0
```

##### In `Julia`:

* we need to use symbolic values for `1` in defining  `_one`:

```jldoctest basicoperations
julia> _one = cos(Sym(1))^2 + sin(Sym(1))^2
   2         2   
cos (1) + sin (1)

julia> (_one - 1).evalf()
-0.e-124
```

```jldoctest basicoperations
julia> (_one - 1).evalf(chop=true)
0
```

----

## `N` with `Julia`

The `N` function is used to convert a symbolic number or boolean into a `Julia` counterpart.

```jldoctest basicoperations
julia> two = Sym(2)
2

julia> a,b,c,d = two, sqrt(two), two^20, two^100
(2, sqrt(2), 1048576, 1267650600228229401496703205376)

julia> N.((a,b,c,d))
(2, 1.414213562373095048801688724209698078569671875376948073176679737990732478462102, 1048576, 1267650600228229401496703205376)
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

```python
    >>> import numpy # doctest:+SKIP
    >>> a = numpy.arange(10) # doctest:+SKIP
    >>> expr = sin(x)
    >>> f = lambdify(x, expr, "numpy") # doctest:+SKIP
    >>> f(a) # doctest:+SKIP
    [ 0.          0.84147098  0.90929743  0.14112001 -0.7568025  -0.95892427
     -0.2794155   0.6569866   0.98935825  0.41211849]
```


!!! note "Alert"
    `lambdify` uses `eval`.  Don't use it on unsanitized input.

##### In `Julia`:

* `lambdify` is defined seperately and with a different argument order: `lambdify(ex, vars=free_symbols(ex))`.

```jldoctest basicoperations
julia> a = 0:10
0:10

julia> @vars x
(x,)

julia> expr = sin(x)
sin(x)

julia> fn = lambdify(expr);

julia> fn.(a)
11-element Array{Float64,1}:
  0.0
  0.8414709848078965
  0.9092974268256817
  0.1411200080598672
 -0.7568024953079282
 -0.9589242746631385
 -0.27941549819892586
  0.6569865987187891
  0.9893582466233818
  0.4121184852417566
 -0.5440211108893698
```

!!!  note "Technical note"
    The `lambdify`  function converts a symbolic  expression into  a `Julia`  expression, and then creates a function using `invokelatest`  to avoid  world  age issues.
	
More performant functions can be produced using the following pattern:
	
```jldoctest basicoperations
julia> ex = sin(x)^2 + x^2
 2      2   
x  + sin (x)

julia> body = convert(Expr, ex)
:(x ^ 2 + sin(x) ^ 2)

julia> syms = Symbol.(free_symbols(ex))
1-element Array{Symbol,1}:
 :x

julia> fn = eval(Expr(:function, Expr(:call, gensym(), syms...), body));

julia> fn(pi)
9.869604401089358
```

----

You can use other libraries than NumPy. For example, to use the standard
library math module, use `"math"`.

```python
    >>> f = lambdify(x, expr, "math")
    >>> f(0.1)
    0.0998334166468
```

##### In `Julia`:

* this doesn't apply, so is not implemented.


----

To use lambdify with numerical libraries that it does not know about, pass a
dictionary of `sympy_name:numerical_function` pairs.  For example

```python
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

```jldoctest basicoperations
julia> mysin(x) = cos(x)
mysin (generic function with 1 method)

julia> ex = sin(x)
sin(x)

julia> body = SymPy.walk_expression(ex, fns=Dict("sin" => :mysin))
:(mysin(x))

julia> syms = (:x,)
(:x,)

julia> fn = eval(Expr(:function, Expr(:call, gensym(), syms...), body));

julia> fn(0)
1.0
```

----

!!! note "TODO"

    Write an advanced numerics section

