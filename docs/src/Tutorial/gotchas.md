# Gotchas

```@setup gotchas
using SymPy
sympy.init_printing(pretty_print=False, use_unicode=False)
```


[From](https://docs.sympy.org/latest/tutorial/gotchas.html)

To begin, we should make something about SymPy clear.  SymPy is nothing more
than a Python library, like `NumPy`, `Django`, or even modules in the
Python standard library `sys` or `re`.  What this means is that SymPy does
not add anything to the Python language.  Limitations that are inherent in the
Python language are also inherent in SymPy.  It also means that SymPy tries to
use Python idioms whenever possible, making programming with SymPy easy for
those already familiar with programming with Python.  As a simple example,
SymPy uses Python syntax to build expressions.  Implicit multiplication (like
`3x` or `3 x`) is not allowed in Python, and thus not allowed in SymPy.
To multiply `3` and `x`, you must type `3*x` with the `*`.


##### In `Julia`:

* implicit multiplication by a *literal* is supported, unlike Python

-----

## Symbols

One consequence of this fact is that SymPy can be used in any environment
where Python is available.  We just import it, like we would any other
library:

```python
    >>> from sympy import *
```

##### In `Julia`:

* the functions from the `sympy` module are loaded with the package:

```jldoctest gotchas
julia> using SymPy
```

----

This imports all the functions and classes from SymPy into our interactive
Python session.  Now, suppose we start to do a computation.

```python
    >>> x + 1
    Traceback (most recent call last):
    ...
    NameError: name 'x' is not defined
```

##### In `Julia`:

* the error output may differ, but an `UndefVarError` is thrown

```julia
julia> x + 1
ERROR: UndefVarError: x not defined
Stacktrace:
 [1] top-level scope at REPL[86]:1
```

----

Oops! What happened here?  We tried to use the variable `x`, but it tells us
that `x` is not defined.  In Python, variables have no meaning until they
are defined.  SymPy is no different.  Unlike many symbolic manipulation
systems you may have used, in SymPy, variables are not defined automatically.
To define variables, we must use `symbols`.

```python
    >>> x = symbols('x')
    >>> x + 1
    x + 1
```

##### In `Julia`:

We can use `symbols`, as here:

```jldoctest gotchas
julia> x = symbols("x")
x

julia> x + 1
x + 1
```

but the recommended way is to use `@syms`:

```jldoctest gotchas
julia> @syms x
(x,)
```

----

`symbols` takes a string of variable names separated by spaces or commas,
and creates Symbols out of them.  We can then assign these to variable names.
Later, we will investigate some convenient ways we can work around this issue.
For now, let us just define the most common variable names, `x`, `y`, and
`z`, for use through the rest of this section

```python
    >>> x, y, z = symbols('x y z')
```

##### In `Julia`:

Again, we use the `@syms` macro:

```jldoctest gotchas
julia> x + 1
x + 1

julia> @syms x, y, z
(x, y, z)
```

----

As a final note, we note that the name of a Symbol and the name of the
variable it is assigned to need not have anything to do with one another.

```python
    >>> a, b = symbols('b a')
    >>> a
    b
    >>> b
    a
```

##### In `Julia`:

The  same  holds:

```jldoctest gotchas
julia>  a, b = symbols("b a")
(b, a)

julia> a
b
```

```jldoctest gotchas
julia> b
a
```

This can also be done with the `@syms` macro:

```jldoctest gotchas
julia> @syms a=>"b" b=>"c"
(b, c)

julia> a + b
b + c
```


----

Here we have done the very confusing thing of assigning a Symbol with the name
`a` to the variable `b`, and a Symbol of the name `b` to the variable
`a`.  Now the Python variable named `a` points to the SymPy Symbol named
`b`, and visa versa.  How confusing.  We could have also done something like

```python
    >>> crazy = symbols('unrelated')
    >>> crazy + 1
    unrelated + 1
```

##### In `Julia`:

```jldoctest gotchas
julia> crazy = symbols("unrelated")
unrelated

julia> crazy + 1
unrelated + 1
```

----

This also shows that Symbols can have names longer than one character if we
want.

Usually, the best practice is to assign Symbols to Python variables of the
same name, although there are exceptions:  Symbol names can contain characters
that are not allowed in Python variable names, or may just want to avoid
typing long names by assigning Symbols with long names to single letter Python
variables.

To avoid confusion, throughout this tutorial, Symbol names and Python variable
names will always coincide.  Furthermore, the word "Symbol" will refer to a
SymPy Symbol and the word "variable" will refer to a Python variable.

Finally, let us be sure we understand the difference between SymPy Symbols and
Python variables.  Consider the following::

```python
  x = symbols('x')
  expr = x + 1
  x = 2
  print(expr)
```


What do you think the output of this code will be?  If you thought `3`,
you're wrong.  Let's see what really happens

```python
    >>> x = symbols('x')
    >>> expr = x + 1
    >>> x = 2
    >>> print(expr)
    x + 1
```

##### In `Julia`:

* we must change to double quotes (or, as recommended, use `@syms x`)

```jldoctest gotchas
julia> x = symbols("x")
x

julia> expr = x + 1
x + 1

julia> x = 2
2

julia> expr
x + 1
```

----

Changing `x` to `2` had no effect on `expr`.  This is because `x = 2`
changes the Python variable `x` to `2`, but has no effect on the SymPy
Symbol `x`, which was what we used in creating `expr`.  When we created
`expr`, the Python variable `x` was a Symbol.  After we created, it, we
changed the Python variable `x` to 2.  But `expr` remains the same.  This
behavior is not unique to SymPy.  All Python programs work this way: if a
variable is changed, expressions that were already created with that variable
do not change automatically.  For example

```python
    >>> x = 'abc'
    >>> expr = x + 'def'
    >>> expr
    'abcdef'
    >>> x = 'ABC'
    >>> expr
    'abcdef'
```

##### In `Julia`:

* The `*` infix operator is used for string concatenation

```jldoctest gotchas
julia> x = "abc"
"abc"

julia> expr = x * "def"
"abcdef"

julia> expr
"abcdef"
```

```jldoctest gotchas
julia> x = "ABC"
"ABC"

julia> expr
"abcdef"
```

----


!!! note "Quick Tip"
    To change the value of a Symbol in an expression, use `subs`

```python
     >>> x = symbols('x')
     >>> expr = x + 1
     >>> expr.subs(x, 2)
     3
```

##### In `Julia`:

```jldoctest gotchas
julia> @syms x
(x,)

julia> expr = x + 1
x + 1

julia> expr.subs(x, 2)
3
```


Or use the "call" form of `subs`: `expr(x => 2)`

----

In this example, if we want to know what `expr` is with the new value of
`x`, we need to reevaluate the code that created `expr`, namely, `expr =
x + 1`.  This can be complicated if several lines created `expr`.  One
advantage of using a symbolic computation system like SymPy is that we can
build a symbolic representation for `expr`, and then substitute `x` with
values.  The correct way to do this in SymPy is to use `subs`, which will be
discussed in more detail later.

```python
    >>> x = symbols('x')
    >>> expr = x + 1
    >>> expr.subs(x, 2)
    3
```

##### In `Julia`:

```jldoctest gotchas
julia> @syms x
(x,)

julia> expr = x + 1
x + 1

julia> expr.subs(x, 2)
3
```

----

!!! note "TODO"
    Add link to basic operations section


## Equals signs

Another very important consequence of the fact that SymPy does not extend
Python syntax is that `=` does not represent equality in SymPy.  Rather it
is Python variable assignment.  This is hard-coded into the Python language,
and SymPy makes no attempts to change that.

You may think, however, that `==`, which is used for equality testing in
Python, is used for SymPy as equality.  This is not quite correct either.  Let
us see what happens when we use `==`.

```python
    >>> x + 1 == 4
    False
```

##### In `Julia`:

* `==` is similar as in Python:
```jldoctest gotchas
julia> x + 1 == 4
false
```

Recall `==` promotes values, so we have a Julia object may be "equal" to a `SymPy` one:

```jldoctest gotchas
julia> 0 == zero(Sym)  ## or Sym(0)
true
```


----

Instead of treating `x + 1 == 4` symbolically, we just got `False`.  In
SymPy, `==` represents exact structural equality testing.  This means that
`a == b` means that we are *asking* if `a = b`.  We always get a `bool` as
the result of `==`.  There is a separate object, called `Eq`, which can be
used to create symbolic equalities

```python
    >>> Eq(x + 1, 4)
    Eq(x + 1, 4)
```

##### In `Julia`:

```jldoctest gotchas
julia> Eq(x + 1, 4)
x + 1 = 4
```

----

There is one additional caveat about `==` as well.  Suppose we want to know
if $(x + 1)^2 = x^2 + 2x + 1$.  We might try something like this:

```python
    >>> (x + 1)**2 == x**2 + 2*x + 1
    False
```

##### In `Julia`:

```jldoctest gotchas
julia> (x + 1)^2 == x^2 + 2*x + 1
false
```

----

We got `False` again. However, $(x + 1)^2$ *does* equal $x^2 + 2x + 1$. What
is going on here?  Did we find a bug in SymPy, or is it just not powerful
enough to recognize this basic algebraic fact?

Recall from above that `==` represents *exact* structural equality testing.
"Exact" here means that two expressions will compare equal with `==` only if
they are exactly equal structurally.  Here, $(x + 1)^2$ and $x^2 + 2x + 1$ are
not the same symbolically. One is the power of an addition of two terms, and
the other is the addition of three terms.

It turns out that when using SymPy as a library, having `==` test for exact
structural equality is far more useful than having it represent symbolic
equality, or having it test for mathematical equality.  However, as a new
user, you will probably care more about the latter two.  We have already seen
an alternative to representing equalities symbolically, `Eq`.  To test if
two things are equal, it is best to recall the basic fact that if `a = b`,
then `a - b = 0`.  Thus, the best way to check if `a = b` is to take `a - b`
and simplify it, and see if it goes to 0.  We will learn :ref:`later
<tutorial-simplify>` that the function to do this is called `simplify`. This
method is not infallible---in fact, it can be `theoretically proven
<http://en.wikipedia.org/wiki/Richardson%27s_theorem>`_ that it is impossible
to determine if two symbolic expressions are identically equal in
general---but for most common expressions, it works quite well.

```python
    >>> a = (x + 1)**2
    >>> b = x**2 + 2*x + 1
    >>> simplify(a - b)
    0
    >>> c = x**2 - 2*x + 1
    >>> simplify(a - c)
    4*x
```

##### In `Julia`:


```jldoctest gotchas
julia> @syms x
(x,)

julia> a = (x + 1)^2; string(a)
"(x + 1)^2"

julia> b = x^2 + 2*x + 1; string(b)
"x^2 + 2*x + 1"

julia> simplify(a - b)
0

julia> c = x^2 - 2*x + 1; string(c)
"x^2 - 2*x + 1"

julia> simplify(a - c)
4⋅x
```

----

There is also a method called `equals` that tests if two expressions are
equal by evaluating them numerically at random points.

```python
    >>> a = cos(x)**2 - sin(x)**2
    >>> b = cos(2*x)
    >>> a.equals(b)
    True
```

##### In `Julia`:

```jldoctest gotchas
julia> a = cos(x)^2 - sin(x)^2
     2         2
- sin (x) + cos (x)

julia> b = cos(2*x)
cos(2⋅x)

julia> a.equals(b)
true
```

----



## Two Final Notes: `^` and `/`

You may have noticed that we have been using `**` for exponentiation instead
of the standard `^`.  That's because SymPy follows Python's conventions.  In
Python, `^` represents logical exclusive or.  SymPy follows this convention:

```python
     >>> True ^ False
     True
     >>> True ^ True
     False
     >>> x^y
     Xor(x, y)
```

##### In `Julia`:

* we export `True` and `False` for symbolic Boolean values

* This does **not** apply, as we use `^` for exponentiation.

* Use the prefix `Or` for logical
```jldoctest gotchas
julia> Or(True, False)
True
```

```jldoctest gotchas
julia> Or(True, True)
True
```

```jldoctest gotchas
julia> Or(x, y)
x ∨ y
```

----

Finally, a small technical discussion on how SymPy works is in order.  When
you type something like `x + 1`, the SymPy Symbol `x` is added to the
Python int `1`.  Python's operator rules then allow SymPy to tell Python
that SymPy objects know how to be added to Python ints, and so `1` is
automatically converted to the SymPy Integer object.

This sort of operator magic happens automatically behind the scenes, and you
rarely need to even know that it is happening.  However, there is one
exception.  Whenever you combine a SymPy object and a SymPy object, or a SymPy
object and a Python object, you get a SymPy object, but whenever you combine
two Python objects, SymPy never comes into play, and so you get a Python
object.

```python
    >>> type(Integer(1) + 1)
    <class 'sympy.core.numbers.Integer'>
    >>> type(1 + 1)
    <... 'int'>
```

##### In `Julia`:

* In Julia, most operations between `SymPy` objects and `Julia` objects will promote to a `SymPy` objects, but of course `Julia` objects combined will produce `Julia` Objects:

```jldoctest gotchas
julia> typeof(sympy.Integer(1) + 1)
Sym
```

```jldoctest gotchas
julia> typeof(1 + 1)
Int64
```

To convert a `Julia` object to a `SymPy` object, the `Sym` constructor may be useful:

```jldoctest gotchas
julia> Sym(1)
1
```

To convert a `SymPy` object to a `Julia` object, the `N` function is useful for numbers and booleans:

```jldoctest gotchas
julia> N(Sym(1)), N(True)
(1, true)
```

And the `lambdify` function can produce a function from an expression:

```jldoctest gotchas
julia> ex = x^2 - 2x + 2
 2
x  - 2⋅x + 2

julia> fn = lambdify(ex);

julia> fn(1) - ex(1)
0
```

----

!!! note
    On running the example above in SymPy Live, (1+1) is wrapped by Integer, so it does not show the correct output.

This is usually not a big deal. Python ints work much the same as SymPy
Integers, but there is one important exception:  division.  In SymPy, the
division of two Integers gives a Rational:

```python
    >>> Integer(1)/Integer(3)
    1/3
    >>> type(Integer(1)/Integer(3))
    <class 'sympy.core.numbers.Rational'>
```

##### In `Julia`:

```jldoctest gotchas
julia> sympy.Integer(1)/sympy.Integer(3)
1/3
```

```jldoctest gotchas
julia> typeof(sympy.Integer(1)/sympy.Integer(3))
Sym
```

And to get the Python, type, we can use `__class__`:

```jldoctest gotchas
julia> (sympy.Integer(1)/sympy.Integer(3)).__class__
PyObject <class 'sympy.core.numbers.Rational'>
```

----

But in Python `/` represents either integer division or floating point
division, depending on whether you are in Python 2 or Python 3, and depending
on whether or not you have run `from __future__ import division`:

```python
    >>> from __future__ import division
    >>> 1/2 #doctest: +SKIP
    0.5
```

##### In `Julia`:

* This does not apply, as `/` is not integer division.

----


To avoid this, we can construct the rational object explicitly

```python
    >>> Rational(1, 2)
    1/2
```

##### In `Julia`:

* `Rational` from `sympy` is *not* exported, it would conflict with `Julia`'s `Rational` costructor. We must qualify it:

```jldoctest gotchas
julia> Rational(1, 2)
1//2
```

```jldoctest gotchas
julia> sympy.Rational(1, 2)
1/2
```

----

This problem also comes up whenever we have a larger symbolic expression with
`int/int` in it.  For example:

```python
    >>> x + 1/2 #doctest: +SKIP
    x + 0.5
```

##### In `Julia`:

* `Int/Int` will produce a floating point value, whereas `Int//Int` will produce a rational, which can then be promoted without loss to a symbolic object:

```jldoctest gotchas
julia> x + 1/2
x + 0.5
```

----

!!! note
   On running the example above in SymPy Live, (1/2) is wrapped
   by Integer, so it does not show the correct output.

This happens because Python first evaluates `1/2` into `0.5`, and then
that is cast into a SymPy type when it is added to `x`.  Again, we can get
around this by explicitly creating a Rational:

```python
    >>> x + Rational(1, 2)
    x + 1/2
```

##### In `Julia`:

```jldoctest gotchas
julia> x + 1//2
x + 1/2
```

----

There are several tips on avoiding this situation in the :ref:`gotchas`
document.

## Further Reading

For more discussion on the topics covered in this section, see :ref:`gotchas`.
