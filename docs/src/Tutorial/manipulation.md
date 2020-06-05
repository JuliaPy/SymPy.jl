# Advanced Expression Manipulation

[From](https://docs.sympy.org/latest/tutorial/manipulation.html)  (version 1.3)

```@setup manipulation
using SymPy
sympy.init_printing(use_unicode=True)
```


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

```python
    >>> from sympy import *
    >>> x, y, z = symbols('x y z')

    >>> expr = 2**x + x*y
    >>> srepr(expr)
    "Add(Pow(Integer(2), Symbol('x')), Mul(Symbol('x'), Symbol('y')))"
```

##### In `Julia`:

* We replace the import command with a `using` command, as this will import functions (not Classes though) from `sympy`


```jldoctest manipulation
julia> using SymPy

julia> x, y, z = symbols("x y z")
(x, y, z)

julia> expr = 2^x + x*y
 x
2  + x⋅y

julia> srepr(expr)
"Add(Pow(Integer(2), Symbol('x')), Mul(Symbol('x'), Symbol('y')))"

```

----

The easiest way to tear this apart is to look at a diagrm of the expression
tree. Here is a [diagram](https://docs.sympy.org/latest/tutorial/manipulation.html#understanding-expression-trees).

!!! note

    This comes from `dotprint(2**x + x*y, labelfunc=srepr)`. But we don't render digraph objects here


First, let's look at the leaves of this tree.  Symbols are instances of the
class Symbol.  While we have been doing

```python
    >>> x = symbols('x')
```

##### In `Julia`:

```jldoctest manipulation
julia> x = symbols("x")
x

```

----

we could have also done

```python
    >>> x = Symbol('x')
```

##### In `Julia`:

* this can be done, but `@vars` would be suggested:

```jldoctest manipulation
julia> x = sympy.Symbol("x")
x

```

```jldoctest manipulation
julia> @vars x
(x,)

```

----
Either way, we get a Symbol with the name "x" [#symbols-fn]_.  For the number in the
expression, 2, we got `Integer(2)`.  `Integer` is the SymPy class for
integers.  It is similar to the Python built-in type `int`, except that
`Integer` plays nicely with other SymPy types.

When we write `2**x`, this creates a `Pow` object.  `Pow` is short for
"power".

```python
    >>> srepr(2**x)
    "Pow(Integer(2), Symbol('x'))"
```

##### In `Julia`:

* we replace `**` by `^`

```jldoctest manipulation
julia> srepr(2^x)
"Pow(Integer(2), Symbol('x'))"

```

----
We could have created the same object by calling `Pow(2, x)`

```python
    >>> Pow(2, x)
    2**x
```

##### In `Julia`:

* `Pow` is *not* a function, rather a managed property, so we must qualify it, as it wasn't brought in when loading the package

```jldoctest manipulation
julia> sympy.Pow(2, x)
 x
2

```

----

Note that in the `srepr` output, we see `Integer(2)`, the SymPy version of
integers, even though technically, we input `2`, a Python int.  In general,
whenever you combine a SymPy object with a non-SymPy object via some function
or operation, the non-SymPy object will be converted into a SymPy object.  The
function that does this is `sympify` [#sympify-fn]_.

```python
    >>> type(2)
    <... 'int'>
    >>> type(sympify(2))
    <class 'sympy.core.numbers.Integer'>
```

##### In `Julia`:

```jldoctest manipulation
julia> typeof(2)
Int64

julia> typeof(sympify(2))
Sym

```

----

We have seen that `2**x` is represented as `Pow(2, x)`.  What about
`x*y`?  As we might expect, this is the multiplication of `x` and `y`.
The SymPy class for multiplication is `Mul`.

```python
    >>> srepr(x*y)
    "Mul(Symbol('x'), Symbol('y'))"
```

##### In `Julia`:

```jldoctest manipulation
julia> srepr(x*y)
"Mul(Symbol('x'), Symbol('y'))"

```

----
Thus, we could have created the same object by writing `Mul(x, y)`.

```python
    >>> Mul(x, y)
    x*y
```

##### In `Julia`:

* Again, `Mul` is not a function, so it must be qualified

```jldoctest manipulation
julia> sympy.Mul(x, y)
x⋅y

```

----
Now we get to our final expression, `2**x + x*y`.  This is the addition of
our last two objects, `Pow(2, x)`, and `Mul(x, y)`.  The SymPy class for
addition is `Add`, so, as you might expect, to create this object, we use
`Add(Pow(2, x), Mul(x, y))`.

```python
    >>> Add(Pow(2, x), Mul(x, y))
    2**x + x*y
```

##### In `Julia`:

* We *can* import these operations to avoid qualifying them as done here:

```jldoctest manipulation
julia> import_from(sympy, (:Add, :Mul, :Pow), typ=:Any)

julia> Add(Pow(2, x), Mul(x, y))
 x
2  + x⋅y

```

----
SymPy expression trees can have many branches, and can be quite deep or quite
broad.  Here is a more complicated example

```python
    >>> expr = sin(x*y)/2 - x**2 + 1/y
    >>> srepr(expr)
    "Add(Mul(Integer(-1), Pow(Symbol('x'), Integer(2))), Mul(Rational(1, 2),
    sin(Mul(Symbol('x'), Symbol('y')))), Pow(Symbol('y'), Integer(-1)))"
```

##### In `Julia`:

```jldoctest manipulation
julia> expr = sin(x*y)/2 - x^2 + 1/y
   2   sin(x⋅y)   1
- x  + ──────── + ─
          2       y

julia> srepr(expr)
"Add(Mul(Integer(-1), Pow(Symbol('x'), Integer(2))), Mul(Rational(1, 2), sin(Mul(Symbol('x'), Symbol('y')))), Pow(Symbol('y'), Integer(-1)))"

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

```python
    >>> srepr(x - y)
    "Add(Symbol('x'), Mul(Integer(-1), Symbol('y')))"
```

##### In `Julia`:

```jldoctest manipulation
julia> srepr(x - y)
"Add(Symbol('x'), Mul(Integer(-1), Symbol('y')))"

```

----

Next, look at `1/y`.  We might expect to see something like `Div(1, y)`,
but similar to subtraction, there is no class in SymPy for division.  Rather,
division is represented by a power of -1.  Hence, we have `Pow(y, -1)`.
What if we had divided something other than 1 by `y`, like `x/y`?  Let's
see.

```python
    >>> expr = x/y
    >>> srepr(expr)
    "Mul(Symbol('x'), Pow(Symbol('y'), Integer(-1)))"
```

##### In `Julia`:

```jldoctest manipulation
julia> expr = x/y
x
─
y

julia> srepr(expr)
"Mul(Symbol('x'), Pow(Symbol('y'), Integer(-1)))"

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

```python
     >>> 1 + x
     x + 1
```

##### In `Julia`:

```jldoctest manipulation
julia> 1 + x
x + 1

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

```python
    >>> expr = Add(x, x)
    >>> expr.func
    <class 'sympy.core.mul.Mul'>
```

##### In `Julia`:

```jldoctest manipulation
julia> expr = Add(x, x)
2⋅x

julia> expr.func
PyObject <class 'sympy.core.mul.Mul'>

```

* The output isn't as desired, as `PyObject`s don't show nicely here. We can ask for the name, which does display as desired:

```jldoctest manipulation
julia> expr.func.__name__
"Mul"

```

* In `SymPy` the `func` and `args` properties are exported as functions in the module `SymPy.Introspection`.


----
We created `Add(x, x)`, so we might expect `expr.func` to be `Add`, but
instead we got `Mul`.  Why is that?  Let's take a closer look at `expr`.

```python
    >>> expr
    2*x
```

##### In `Julia`:

```jldoctest manipulation
julia> expr
2⋅x

```

----

`Add(x, x)`, i.e., `x + x`, was automatically converted into `Mul(2,
x)`, i.e., `2*x`, which is a `Mul`.   SymPy classes make heavy use of the
`__new__` class constructor, which, unlike `__init__`, allows a different
class to be returned from the constructor.

Second, some classes are special-cased, usually for efficiency reasons
[#singleton-fn]_.

```python
    >>> Integer(2).func
    <class 'sympy.core.numbers.Integer'>
    >>> Integer(0).func
    <class 'sympy.core.numbers.Zero'>
    >>> Integer(-1).func
    <class 'sympy.core.numbers.NegativeOne'>
```

##### In `Julia`:

```jldoctest manipulation
julia> sympy.Integer(2).func.__name__
"Integer"

julia> sympy.Integer(0).func.__name__
"Zero"

julia> sympy.Integer(-1).func.__name__
"NegativeOne"

```

----
For the most part, these issues will not bother us.  The special classes
`Zero`, `One`, `NegativeOne`, and so on are subclasses of `Integer`,
so as long as you use `isinstance`, it will not be an issue.

### args


`args` are the top-level arguments of the object.  `(x*y).args` would be
`(x, y)`.  Let's look at some examples

```python
    >>> expr = 3*y**2*x
    >>> expr.func
    <class 'sympy.core.mul.Mul'>
    >>> expr.args
    (3, x, y**2)
```

##### In `Julia`:

* The `args` property can be accessed exactly as `func`

```jldoctest manipulation
julia> expr = 3*y^2*x
     2
3⋅x⋅y

julia> expr.func.__name__
"Mul"

julia> expr.args
(3, x, y^2)

```

----
From this, we can see that `expr == Mul(3, y**2, x)`.  In fact, we can see
that we can completely reconstruct `expr` from its `func` and its
`args`.

```python
    >>> expr.func(*expr.args)
    3*x*y**2
    >>> expr == expr.func(*expr.args)
    True
```

##### In `Julia`:

```jldoctest manipulation
julia> expr.func(expr.args...)
     2
3⋅x⋅y

julia> expr == expr.func(expr.args...)
true

```

----
Note that although we entered `3*y**2*x`, the `args` are `(3, x, y**2)`.
In a `Mul`, the Rational coefficient will come first in the `args`, but
other than that, the order of everything else follows no special pattern.  To
be sure, though, there is an order.

```python
    >>> expr = y**2*3*x
    >>> expr.args
    (3, x, y**2)
```

##### In `Julia`:

```jldoctest manipulation
julia> expr = y^2*3*x
     2
3⋅x⋅y

julia> expr.args
(3, x, y^2)
```

----
Mul's `args` are sorted, so that the same `Mul` will have the same
`args`.  But the sorting is based on some criteria designed to make the
sorting unique and efficient that has no mathematical significance.

The `srepr` form of our `expr` is `Mul(3, x, Pow(y, 2))`.  What if we
want to get at the `args` of `Pow(y, 2)`.  Notice that the `y**2` is in
the third slot of `expr.args`, i.e., `expr.args[2]`.

```python
    >>> expr.args[2]
    y**2
```

##### In `Julia`:

```jldoctest manipulation
julia> expr.args[2]
x

```

----
So to get the `args` of this, we call `expr.args[2].args`.

```python
    >>> expr.args[2].args
    (y, 2)
```

##### In `Julia`:

* Python uses 0-based indexing, so we bump the index by 1

```jldoctest manipulation
julia> expr.args[3].args
(y, 2)

```

----
Now what if we try to go deeper.  What are the args of `y`.  Or `2`.
Let's see.

```python
    >>> y.args
    ()
    >>> Integer(2).args
    ()
```

##### In `Julia`:

```jldoctest manipulation
julia> y.args
()

julia> sympy.Integer(2).args
()

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

```python
    >>> def pre(expr):
    ...     print(expr)
    ...     for arg in expr.args:
    ...         pre(arg)
```

##### In `Julia`:

```jldoctest manipulation
julia> function pre(expr)
       @show expr
       for arg in expr.args
         pre(arg)
       end
       end
pre (generic function with 1 method)
```

----

See how nice it is that `()` signals leaves in the expression tree.  We
don't even have to write a base case for our recursion; it is handled
automatically by the for loop.

Let's test our function.

```python
    >>> expr = x*y + 1
    >>> pre(expr)
    x*y + 1
    1
    x*y
    x
    y
```

##### In `Julia`:

* Here we see the output:

```jldoctest manipulation
julia> expr = x*y + 1
x⋅y + 1

julia> pre(expr)
expr = x*y + 1
expr = 1
expr = x*y
expr = x
expr = y

```

----

Can you guess why we called our function `pre`?  We just wrote a pre-order
traversal function for our expression tree.   See if you can write a
post-order traversal function.

Such traversals are so common in SymPy that the generator functions
`preorder_traversal` and `postorder_traversal` are provided to make such
traversals easy.  We could have also written our algorithm as

```python
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

```jldoctest manipulation
julia> for arg in sympy.preorder_traversal(expr)
         @show arg
       end
arg = x*y + 1
arg = 1
arg = x*y
arg = x
arg = y

```

----

## Prevent expression evaluation


There are generally two ways to prevent the evaluation, either pass an
`evaluate=False` parameter while constructing the expression, or create
an evaluation stopper by wrapping the expression with `UnevaluatedExpr`.

For example:

```python
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

```jldoctest manipulation
julia> @vars x y z
(x, y, z)

julia> x + x
2⋅x

julia> Add(x, x)
2⋅x

julia> Add(x, x, evaluate=False)
x + x

```

----
If you don't remember the class corresponding to the expression you
want to build (operator overloading usually assumes `evaluate=True`),
just use `sympify` and pass a string:

```python
    >>> from sympy import sympify
    >>> sympify("x + x", evaluate=False)
    x + x
```

##### In `Julia`:



```jldoctest manipulation
julia> sympify("x + x", evaluate=false)
2⋅x

```

----
Note that `evaluate=False` won't prevent future evaluation in later
usages of the expression:

```python
    >>> expr = Add(x, x, evaluate=False)
    >>> expr
    x + x
    >>> expr + x
    3*x
```

##### In `Julia`:

```jldoctest manipulation
julia> expr = Add(x, x, evaluate=false)
x + x

```

```jldoctest manipulation
julia> expr + x
3⋅x
```

----
That's why the class `UnevaluatedExpr` comes handy.
`UnevaluatedExpr` is a method provided by SymPy which lets the user keep
an expression unevaluated. By *unevaluated* it is meant that the value
inside of it will not interact with the expressions outside of it to give
simplified outputs. For example:

```python
    >>> from sympy import UnevaluatedExpr
    >>> expr = x + UnevaluatedExpr(x)
    >>> expr
    x + x
    >>> x + expr
    2*x + x
```

##### In `Julia`:

```jldoctest manipulation
julia> import_from(sympy, (:UnevaluatedExpr,))

julia> expr
x + x

julia> x + expr
3⋅x

```

----
The `x` remaining alone is the `x` wrapped by `UnevaluatedExpr`.
To release it:

```python
    >>> (x + expr).doit()
    3*x
```

##### In `Julia`:

```jldoctest manipulation
julia> (x + expr).doit()
3⋅x

```

----
Other examples:

```python
    >>> from sympy import *
    >>> from sympy.abc import x, y, z
    >>> uexpr = UnevaluatedExpr(S.One*5/7)*UnevaluatedExpr(S.One*3/4)
    >>> uexpr
    (5/7)*(3/4)
    >>> x*UnevaluatedExpr(1/x)
    x*1/x
```

##### In `Julia`:

```jldoctest manipulation
julia> @vars x y z
(x, y, z)

julia> const S = sympy.S
PyObject S

julia> uexpr = UnevaluatedExpr(S.One * 5/7) * UnevaluatedExpr(S.One * 3/4)
5/7⋅3/4

julia> x * UnevaluatedExpr(1/x)
  1
x⋅─
  x

```

----

A point to be noted is that  `UnevaluatedExpr` cannot prevent the
evaluation of an expression which is given as argument. For example:

```python
    >>> expr1 = UnevaluatedExpr(x + x)
    >>> expr1
    2*x
    >>> expr2 = sympify('x + x', evaluate=False)
    >>> expr2
    x + x
```

##### In `Julia`:

```jldoctest manipulation
julia> expr1 = UnevaluatedExpr(x + x)
2⋅x

julia> expr2 = sympify("x + x", evaluate=False)
2⋅x

```

----


Remember that `expr2` will be evaluated if included into another
expression. Combine both of the methods to prevent both inside and outside
evaluations:

```python
    >>> UnevaluatedExpr(sympify("x + x", evaluate=False)) + y
    y + x + x
```

##### In `Julia`:

```jldoctest manipulation
julia> UnevaluatedExpr(sympify("x + x", evaluate=False)) + y
y + 2⋅x
```

----

`UnevalutedExpr` is supported by SymPy printers and can be used to print the
result in different output forms. For example

```python
    >>> from sympy import latex
    >>> uexpr = UnevaluatedExpr(S.One*5/7)*UnevaluatedExpr(S.One*3/4)
    >>> print(latex(uexpr))
    \frac{5}{7} \frac{3}{4}
```

##### In `Julia`:

* The printing support is through `show`, but we can use SymPy's:

```jldoctest manipulation
julia> uexpr = UnevaluatedExpr(S.One*5/7)*UnevaluatedExpr(S.One*3/4)
5/7⋅3/4

julia> sympy.latex(uexpr)
"\\frac{5}{7} \\frac{3}{4}"


```

----

In order to release the expression and get the evaluated LaTeX form,
just use `.doit()`:

```python
    >>> print(latex(uexpr.doit()))
    \frac{15}{28}
```

##### In `Julia`:

```jldoctest manipulation
julia> sympy.latex(uexpr.doit())
"\\frac{15}{28}"

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
