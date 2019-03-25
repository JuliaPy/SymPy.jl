# Advanced Expression Manipulation

In this section, we discuss some ways that we can perform advanced
manipulation of expressions.

## Understanding Expression Trees


!!! note "Quick Tip"

   To play with the `srepr` form of expressions in the SymPy Live shell,
   change the output format to `Repr` in the settings.

Before we can do this, we need to understand how expressions are represented
in SymPy.  A mathematical expression is represented as a tree.  Let us take
the expression `2^x + xy`, i.e., `2**x + x*y`.  We can see what this
expression looks like internally by using `srepr`

```verbatim
    >>> from sympy import *
    >>> x, y, z = symbols('x y z')

    >>> expr = 2**x + x*y
    >>> srepr(expr)
    "Add(Pow(Integer(2), Symbol('x')), Mul(Symbol('x'), Symbol('y')))"
```

##### In `Julia`:

* We replace the import command with a `using` command, as this will import functions (not Classes though) from `sympy`


```
using SymPy
x, y, z = symbols("x y z")

expr = 2^x + x*y
srepr(expr)
```

----

The easiest way to tear this apart is to look at a diagrm of the expression
tree. Here is a [diagram](https://docs.sympy.org/latest/tutorial/manipulation.html#understanding-expression-trees).

!!! note

    This comes from `dotprint(2**x + x*y, labelfunc=srepr)`. But we don't render digraph objects here


First, let's look at the leaves of this tree.  Symbols are instances of the
class Symbol.  While we have been doing

```verbatim
    >>> x = symbols('x')
```

##### In `Julia`:

```
x = symbols("x")
```

----

we could have also done

```verbatim
    >>> x = Symbol('x')
```

##### In `Julia`:

* this can be done, but `@vars` would be suggested:

```
x = sympy.Symbol("x")
```

```
@vars x
```

----
Either way, we get a Symbol with the name "x" [#symbols-fn]_.  For the number in the
expression, 2, we got `Integer(2)`.  `Integer` is the SymPy class for
integers.  It is similar to the Python built-in type `int`, except that
`Integer` plays nicely with other SymPy types.

When we write `2**x`, this creates a `Pow` object.  `Pow` is short for
"power".

```verbatim
    >>> srepr(2**x)
    "Pow(Integer(2), Symbol('x'))"
```

##### In `Julia`:

* we replace `**` by `^`

```
srepr(2^x)
```

----
We could have created the same object by calling `Pow(2, x)`

```verbatim
    >>> Pow(2, x)
    2**x
```

##### In `Julia`:

* `Pow` is *not* a function, rather a managed property, so we must qualify it, as it wasn't brought in when loading the package

```
sympy.Pow(2, x)
```

----

Note that in the `srepr` output, we see `Integer(2)`, the SymPy version of
integers, even though technically, we input `2`, a Python int.  In general,
whenever you combine a SymPy object with a non-SymPy object via some function
or operation, the non-SymPy object will be converted into a SymPy object.  The
function that does this is `sympify` [#sympify-fn]_.

```verbatim
    >>> type(2)
    <... 'int'>
    >>> type(sympify(2))
    <class 'sympy.core.numbers.Integer'>
```

##### In `Julia`:

```
typeof(2)
```

```
typeof(sympify(2))
```

----

We have seen that `2**x` is represented as `Pow(2, x)`.  What about
`x*y`?  As we might expect, this is the multiplication of `x` and `y`.
The SymPy class for multiplication is `Mul`.

```verbatim
    >>> srepr(x*y)
    "Mul(Symbol('x'), Symbol('y'))"
```

##### In `Julia`:

```
srepr(x*y)
```

----
Thus, we could have created the same object by writing `Mul(x, y)`.

```verbatim
    >>> Mul(x, y)
    x*y
```

##### In `Julia`:

* Again, `Mul` is not a function, so it must be qualified

```
sympy.Mul(x, y)
```

----
Now we get to our final expression, `2**x + x*y`.  This is the addition of
our last two objects, `Pow(2, x)`, and `Mul(x, y)`.  The SymPy class for
addition is `Add`, so, as you might expect, to create this object, we use
`Add(Pow(2, x), Mul(x, y))`.

```verbatim
    >>> Add(Pow(2, x), Mul(x, y))
    2**x + x*y
```

##### In `Julia`:

* We *can* import these operations to avoid qualifying them as done here:

```
import_from(sympy, (:Add, :Mul, :Pow))
Add(Pow(2, x), Mul(x, y))
```

----
SymPy expression trees can have many branches, and can be quite deep or quite
broad.  Here is a more complicated example

```verbatim
    >>> expr = sin(x*y)/2 - x**2 + 1/y
    >>> srepr(expr)
    "Add(Mul(Integer(-1), Pow(Symbol('x'), Integer(2))), Mul(Rational(1, 2),
    sin(Mul(Symbol('x'), Symbol('y')))), Pow(Symbol('y'), Integer(-1)))"
```

##### In `Julia`:

```
expr = sin(x*y)/2 - x^2 + 1/y
srepr(expr)
```

----
Here is a [diagram](https://docs.sympy.org/latest/tutorial/manipulation.html#understanding-expression-trees)

!!! note

    produced with `dotprint(sin(x*y)/2 - x**2 + 1/y, labelfunc=srepr)`, but not rendered here


This expression reveals some interesting things about SymPy expression
trees. Let's go through them one by one.

Let's first look at the term `x**2`.  As we expected, we see `Pow(x, 2)`.
One level up, we see we have `Mul(-1, Pow(x, 2))`.  There is no subtraction
class in SymPy.  `x - y` is represented as `x + -y`, or, more completely,
`x + -1*y`, i.e., `Add(x, Mul(-1, y))`.

```verbatim
    >>> srepr(x - y)
    "Add(Symbol('x'), Mul(Integer(-1), Symbol('y')))"
```

##### In `Julia`:

```
srepr(x - y)
```

----

Next, look at `1/y`.  We might expect to see something like `Div(1, y)`,
but similar to subtraction, there is no class in SymPy for division.  Rather,
division is represented by a power of -1.  Hence, we have `Pow(y, -1)`.
What if we had divided something other than 1 by `y`, like `x/y`?  Let's
see.

```verbatim
    >>> expr = x/y
    >>> srepr(expr)
    "Mul(Symbol('x'), Pow(Symbol('y'), Integer(-1)))"
```

##### In `Julia`:

```
expr = x/y
srepr(expr)
```

----

We see that `x/y` is represented as `x*y**-1`, i.e., `Mul(x, Pow(y,
-1))`.

Finally, let's look at the `sin(x*y)/2` term.  Following the pattern of the
previous example, we might expect to see `Mul(sin(x*y), Pow(Integer(2),
-1))`.  But instead, we have `Mul(Rational(1, 2), sin(x*y))`.  Rational
numbers are always combined into a single term in a multiplication, so that
when we divide by 2, it is represented as multiplying by 1/2.

Finally, one last note.  You may have noticed that the order we entered our
expression and the order that it came out from `srepr` or in the graph were
different.  You may have also noticed this phenomenon earlier in the
tutorial.  For example

```verbatim
     >>> 1 + x
     x + 1
```

##### In `Julia`:

```
1 + x
```

----
This because in SymPy, the arguments of the commutative operations `Add` and
`Mul` are stored in an arbitrary (but consistent!) order, which is
independent of the order inputted (if you're worried about noncommutative
multiplication, don't be.  In SymPy, you can create noncommutative Symbols
using `Symbol('A', commutative=False)`, and the order of multiplication for
noncommutative Symbols is kept the same as the input).  Furthermore, as we
shall see in the next section, the printing order and the order in which
things are stored internally need not be the same either.

!!! note "Quick Tip"

   The way an expression is represented internally and the way it is printed
   are often not the same.

In general, an important thing to keep in mind when working with SymPy expression
trees is this:  the internal representation of an expression and the way it is
printed need not be the same.  The same is true for the input form.   If some
expression manipulation algorithm is not working in the way you expected it
to, chances are, the internal representation of the object is different from
what you thought it was.

## Recursing through an Expression Tree


Now that you know how expression trees work in SymPy, let's look at how to dig
our way through an expression tree.  Every object in SymPy has two very
important attributes, `func`, and `args`.


### `func`

`func` is the head of the object. For example, `(x*y).func` is `Mul`.
Usually it is the same as the class of the object (though there are exceptions
to this rule).

Two notes about `func`.  First, the class of an object need not be the same
as the one used to create it.  For example

```verbatim
    >>> expr = Add(x, x)
    >>> expr.func
    <class 'sympy.core.mul.Mul'>
```

##### In `Julia`:

```
expr = Add(x, x)
expr.func
```

* The output isn't as desired, as `PyObject`s don't show nicely here. We can ask for the name, which does display as desired:

```
expr.func.__name__
```

* In `SymPy` the `func` and `args` properties are exported as functions in the module `SymPyLite.Introspection`.


----
We created `Add(x, x)`, so we might expect `expr.func` to be `Add`, but
instead we got `Mul`.  Why is that?  Let's take a closer look at `expr`.

```verbatim
    >>> expr
    2*x
```

##### In `Julia`:

```
expr
```

----

`Add(x, x)`, i.e., `x + x`, was automatically converted into `Mul(2,
x)`, i.e., `2*x`, which is a `Mul`.   SymPy classes make heavy use of the
`__new__` class constructor, which, unlike `__init__`, allows a different
class to be returned from the constructor.

Second, some classes are special-cased, usually for efficiency reasons
[#singleton-fn]_.

```verbatim
    >>> Integer(2).func
    <class 'sympy.core.numbers.Integer'>
    >>> Integer(0).func
    <class 'sympy.core.numbers.Zero'>
    >>> Integer(-1).func
    <class 'sympy.core.numbers.NegativeOne'>
```

##### In `Julia`:

```
sympy.Integer(2).func.__name__
```

```
sympy.Integer(0).func.__name__
```

```
sympy.Integer(-1).func.__name__
```

----
For the most part, these issues will not bother us.  The special classes
`Zero`, `One`, `NegativeOne`, and so on are subclasses of `Integer`,
so as long as you use `isinstance`, it will not be an issue.

### args


`args` are the top-level arguments of the object.  `(x*y).args` would be
`(x, y)`.  Let's look at some examples

```verbatim
    >>> expr = 3*y**2*x
    >>> expr.func
    <class 'sympy.core.mul.Mul'>
    >>> expr.args
    (3, x, y**2)
```

##### In `Julia`:

* The `args` property can be accessed exactly as `func`

```
expr = 3*y^2*x
expr.func.__name__
```

```
expr.args
```

----
From this, we can see that `expr == Mul(3, y**2, x)`.  In fact, we can see
that we can completely reconstruct `expr` from its `func` and its
`args`.

```verbatim
    >>> expr.func(*expr.args)
    3*x*y**2
    >>> expr == expr.func(*expr.args)
    True
```

##### In `Julia`:

```
expr.func(expr.args...)
```

```
expr == expr.func(expr.args...)
```

----
Note that although we entered `3*y**2*x`, the `args` are `(3, x, y**2)`.
In a `Mul`, the Rational coefficient will come first in the `args`, but
other than that, the order of everything else follows no special pattern.  To
be sure, though, there is an order.

```verbatim
    >>> expr = y**2*3*x
    >>> expr.args
    (3, x, y**2)
```

##### In `Julia`:

```
expr = y^2*3*x
expr.args
```

----
Mul's `args` are sorted, so that the same `Mul` will have the same
`args`.  But the sorting is based on some criteria designed to make the
sorting unique and efficient that has no mathematical significance.

The `srepr` form of our `expr` is `Mul(3, x, Pow(y, 2))`.  What if we
want to get at the `args` of `Pow(y, 2)`.  Notice that the `y**2` is in
the third slot of `expr.args`, i.e., `expr.args[2]`.

```verbatim
    >>> expr.args[2]
    y**2
```

##### In `Julia`:

```
expr.args[2]
```

----
So to get the `args` of this, we call `expr.args[2].args`.

```verbatim
    >>> expr.args[2].args
    (y, 2)
```

##### In `Julia`:

* Python uses 0-based indexing, so we bump the index by 1

```
expr.args[3].args
```

----
Now what if we try to go deeper.  What are the args of `y`.  Or `2`.
Let's see.

```verbatim
    >>> y.args
    ()
    >>> Integer(2).args
    ()
```

##### In `Julia`:

```
y.args
```

```
sympy.Integer(2).args
```

----
They both have empty `args`.  In SymPy, empty `args` signal that we have
hit a leaf of the expression tree.

So there are two possibilities for a SymPy expression. Either it has empty
`args`, in which case it is a leaf node in any expression tree, or it has
`args`, in which case, it is a branch node of any expression tree.  When it
has `args`, it can be completely rebuilt from its `func` and its `args`.
This is expressed in the key invariant.

!!! note "Key Invariant"

   Every well-formed SymPy expression must either have empty `args` or
   satisfy `expr == expr.func(expr.args...)`.

(Recall that in Python if `a` is a tuple, then `f(*a)` means to call `f`
with arguments from the elements of `a`, e.g., `f(*(1, 2, 3))` is the same
as `f(1, 2, 3)`.)

This key invariant allows us to write simple algorithms that walk expression
trees, change them, and rebuild them into new expressions.


##### In `Julia`:

* Splatting replaces the `*a` term above, or `f(a...)`.

----

### Walking the Tree


With this knowledge, let's look at how we can recurse through an expression
tree.  The nested nature of `args` is a perfect fit for recursive functions.
The base case will be empty `args`.  Let's write a simple function that goes
through an expression and prints all the `args` at each level.

```verbatim
    >>> def pre(expr):
    ...     print(expr)
    ...     for arg in expr.args:
    ...         pre(arg)
```

##### In `Julia`:

```
function pre(expr)
    @show expr
    for arg in expr.args
    	pre(arg)
	end
end
```

----

See how nice it is that `()` signals leaves in the expression tree.  We
don't even have to write a base case for our recursion; it is handled
automatically by the for loop.

Let's test our function.

```verbatim
    >>> expr = x*y + 1
    >>> pre(expr)
    x*y + 1
    1
    x*y
    x
    y
```

##### In `Julia`:

* `@show` does not work in these notes; this would need copy-and-pasting to be verified

```
expr = x*y + 1
pre(expr)
```

----

Can you guess why we called our function `pre`?  We just wrote a pre-order
traversal function for our expression tree.   See if you can write a
post-order traversal function.

Such traversals are so common in SymPy that the generator functions
`preorder_traversal` and `postorder_traversal` are provided to make such
traversals easy.  We could have also written our algorithm as

```verbatim
    >>> for arg in preorder_traversal(expr):
    ...     print(arg)
    x*y + 1
    1
    x*y
    x
    y
```

##### In `Julia`:

* The `preorder_traversal` function is not a function, so needs to be qualified:

```
for arg in sympy.preorder_traversal(expr)
   @show arg
end
```

----

## Prevent expression evaluation


There are generally two ways to prevent the evaluation, either pass an
`evaluate=False` parameter while constructing the expression, or create
an evaluation stopper by wrapping the expression with `UnevaluatedExpr`.

For example:

```verbatim
    >>> from sympy import Add
    >>> from sympy.abc import x, y, z
    >>> x + x
    2*x
    >>> Add(x, x)
    2*x
    >>> Add(x, x, evaluate=False)
    x + x
```

##### In `Julia`:

```
@vars x y z
x + x
```

```
Add(x, x)
```

```
Add(x, x, evaluate=False)
```

----
If you don't remember the class corresponding to the expression you
want to build (operator overloading usually assumes `evaluate=True`),
just use `sympify` and pass a string:

```verbatim
    >>> from sympy import sympify
    >>> sympify("x + x", evaluate=False)
    x + x
```

##### In `Julia`:



```
sympify("x + x", evaluate=false)
    x + x
```

----
Note that `evaluate=False` won't prevent future evaluation in later
usages of the expression:

```verbatim
    >>> expr = Add(x, x, evaluate=False)
    >>> expr
    x + x
    >>> expr + x
    3*x
```

##### In `Julia`:

```
expr = Add(x, x, evaluate=false)
expr
```

```
expr + x
```

----
That's why the class `UnevaluatedExpr` comes handy.
`UnevaluatedExpr` is a method provided by SymPy which lets the user keep
an expression unevaluated. By *unevaluated* it is meant that the value
inside of it will not interact with the expressions outside of it to give
simplified outputs. For example:

```verbatim
    >>> from sympy import UnevaluatedExpr
    >>> expr = x + UnevaluatedExpr(x)
    >>> expr
    x + x
    >>> x + expr
    2*x + x
```

##### In `Julia`:

```
import_from(sympy, (:UnevaluatedExpr,))
expr = x + UnevaluatedExpr(x)
expr
```

```
x + expr
```

----
The `x` remaining alone is the `x` wrapped by `UnevaluatedExpr`.
To release it:

```verbatim
    >>> (x + expr).doit()
    3*x
```

##### In `Julia`:

```
(x + expr).doit()
```

----
Other examples:

```verbatim
    >>> from sympy import *
    >>> from sympy.abc import x, y, z
    >>> uexpr = UnevaluatedExpr(S.One*5/7)*UnevaluatedExpr(S.One*3/4)
    >>> uexpr
    (5/7)*(3/4)
    >>> x*UnevaluatedExpr(1/x)
    x*1/x
```

##### In `Julia`:

```
@vars x y z
const S = sympy.S
uexpr = UnevaluatedExpr(S.One * 5/7) * UnevaluatedExpr(S.One * 3/4)
uexpr
```

```
x * UnevaluatedExpr(1/x)
```

----

A point to be noted is that  `UnevaluatedExpr` cannot prevent the
evaluation of an expression which is given as argument. For example:

```verbatim
    >>> expr1 = UnevaluatedExpr(x + x)
    >>> expr1
    2*x
    >>> expr2 = sympify('x + x', evaluate=False)
    >>> expr2
    x + x
```

##### In `Julia`:

```
expr1 = UnevaluatedExpr(x + x)
expr1
```

```
expr2 = sympify("x + x", evaluate=False)
expr2
```

----


Remember that `expr2` will be evaluated if included into another
expression. Combine both of the methods to prevent both inside and outside
evaluations:

```verbatim
    >>> UnevaluatedExpr(sympify("x + x", evaluate=False)) + y
    y + x + x
```

##### In `Julia`:

```
UnevaluatedExpr(sympify("x + x", evaluate=False)) + y
```

----

`UnevalutedExpr` is supported by SymPy printers and can be used to print the
result in different output forms. For example

```verbatim
    >>> from sympy import latex
    >>> uexpr = UnevaluatedExpr(S.One*5/7)*UnevaluatedExpr(S.One*3/4)
    >>> print(latex(uexpr))
    \frac{5}{7} \frac{3}{4}
```

##### In `Julia`:

* The printing support is through `show`, but we can use SymPy's:

```
uexpr = UnevaluatedExpr(S.One*5/7)*UnevaluatedExpr(S.One*3/4)
sympy.latex(uexpr)
```

----

In order to release the expression and get the evaluated LaTeX form,
just use `.doit()`:

```verbatim
    >>> print(latex(uexpr.doit()))
    \frac{15}{28}
```

##### In `Julia`:

```
sympy.latex(uexpr.doit())
```

----

!!! note "Footnotes"

* [#symbols-fn] We have been using `symbols` instead of `Symbol` because it
  automatically splits apart strings into multiple `Symbol`\ s.
  `symbols('x y z')` returns a tuple of three `Symbol`\ s.  `Symbol('x y
  z')` returns a single `Symbol` called `x y z`.

* [#sympify-fn] Technically, it is an internal function called `_sympify`,
  which differs from `sympify` in that it does not convert strings.  `x +
  '2'` is not allowed.

* [#singleton-fn] Classes like `One` and `Zero` are singletonized, meaning
  that only one object is ever created, no matter how many times the class is
  called.  This is done for space efficiency, as these classes are very
  common.  For example, `Zero` might occur very often in a sparse matrix
  represented densely.  As we have seen, `NegativeOne` occurs any time we
  have `-x` or `1/x`.  It is also done for speed efficiency because
  singletonized objects can be compared by `is`.  The unique objects for
  each singletonized class can be accessed from the `S` object.
