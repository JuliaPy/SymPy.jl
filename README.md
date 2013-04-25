## Package to bring `sympy` functionality into `julia` via `PyCall`

The `SymPy` package  (`http://sympy.org/`)  is a Python library for symbolic mathematics. 

With the excellent `PyCall` package of `julia`, one has access to the many features of `SymPy` from a `julia` session.

This `SymPy` package provides a light interface for _some_ of the features of `SymPy` that makes working with `SymPy` objects a bit easier.

To use it, both `Python` and the `SymPy` package must be installed on your system.


## The `PyCall` interface to `SymPy`

The only point to this package is that using `PyCall` to access `SymPy` is somewhat cumbersome. The following is how one would define a symbolic value `x`, take its sine, then evaluate at `pi`, say:

```
using PyCall
@pyimport sympy
x = sympy.Symbol("x")
y = sympy.sin(x)
y[:subs](x, pi) | float
```

The `Symbol` and `sin` function of  `SymPy` are found within the imported `sympy` object. They may be referenced with `Python`'s dot notation. However, the  `subs` method of the `y` object is accessed differently, using indexing notation with a symbol. The call above substitutes a value of `pi` for `x`. This leaves the object as a `PyObject` storing a number which can be brought back into `julia` through conversion, in this case with the `float` function.

The above isn't so awkward, but even more cumbersome is the similarly simple task of finding `sin(pi*x)`.
As this multiplication is done at the python level and is not a method of `sympy` or the `x` object, we need to evaluate python code. Here is one solution:

```
x = sympy.Symbol("x")
y = pyeval("k*x", k=pi, =x)     # PyObject 3.14159265358979*x
z = sympy.sin(y)		# PyObject sin(3.14159265358979*x)
z[:subs](x, 1) | float		# 1.2246467991473532e-16
```

This gets replaced by a more `julia`n syntax:

```
using SymPy
x = sym"x"			# or Sym("x") or Sym(:x)
y = sin(pi*x)
subs(y, x, 1)
```

The object `x` we create is of type `Sym`, a simple proxy for the underlying `PyObject`. We then overload the familiar math functions so that working with symbolic expressions can use natural `julia` idioms.

However, the `PyCall` interface is needed for serious work, as only a small portion of the `SymPy` interface is exposed.  To dig the `PyObject` out of a `Sym` object, you access its property `x`, as in `y.x`. This is necessary if passing a `Sym` object to a method call.

## Examples

To make a symbolic object (of type `Sym`) we have the `Sym` constructor and the convenient `sym` macro:

```
x = Sym("x")
h, y = Sym("h", :y)
```

Operator overloading of the basic math functions allows symbolic expressions to be combined without fuss:

```
1/x + 1/x^2 + x			# pretty prints x + 1/x + 1/x^2
```


The `together` functions combines terms:

```
together(1/x + 1/x^2 + x)	# (x^3 + x + 1) / x^2
```

The `expand` function breaks them up;

```
expand( (x + 1)*(x + 2) )	# x^2 + 3 * x + 2
```

The `apart` function does partial fraction decomposition:

```
apart(1/(x +2)/(x + 1))		# -1/(x+2) + 1/(x+1)
```


The `subs` command is used to substitute values. These values are typically numeric, though they may be other symbols:

```
subs(x + y, x, 3)		# y + 3
subs(x*y, y,  24 - 2x) 		# x*(-2*x + 24)
```

Somehow that syntax isn't so natural. We introduce the following non-`SymPy` construction to mimic some math notation used with integration:

```
x*y | (y == 3)
```

(The parentheses are necessary due to the order of operations in `julia`. To do this, we overload `==` for the pair (`Sym`, `Union(Real, Complex`) which doesn't really make sense to use for comparison anyways.)

### Printing

By default, `SymPy`'s pretty printing is used. This works well, except for arrays, which get kind of clunky. The `_str` function can give a more basic output (from `Python`'s `__str__` method for objects):

```
julia> 1/x
1
─
x

julia> [1/x, 1/x^2]
2-element Sym Array:
 1
─
x      
 1 
──
 2
x 

julia> _str([1/x, 1/x^2])
2-element ASCIIString Array:
 "1/x"    
 "x**(-2)"
```

### Plotting

Plotting is done with the `GoogleCharts` package. This is temporary
while other plotting interfaces stabilize. We can plot expressions or
vectors of expressions (in figure):

```
plot(exp(-x) * sin(x), 0, 2pi)	# opens plot in browser
plot( [sin(x), diff(sin(x), x) ], 0, 2pi)
```

<img src="charts-plot.png"></img>

## Conversion

The key to plotting working is forming a function object from the `Sym` object.
Here is one pattern to do so where we need to be careful to match the
variable we substitute with (`x`) with the expression.

```
u -> float(subs( exp(-x) * sin(x), x, u)) # anonymous function to evaluate expression
```

This is what happens in the call: `convert(Function, exp(-x) * sin(x))`. This can be used to plot expressions with `julia`'s other plotting packages.

Basic conversions from `SymPy` numeric types to the corresponding `julia` objects may be done with the functions `integer`, `float`, and `complex`. Rational expressions can be converted through `convert(Rational, ex )`. 


The `sympy` function `n` can be used to form a numerical value from an expression, though the expression is still a `Sym` instance. 

```
sqrt(x + 1) | (x == 2)		# pretty print sqrt(3)
sqrt(x + 1) | (x == 2) | n	# 1.73205080756888 as Sym object
```




The `n` function takes an argument to control the number of digits. For example, we can do 

```
sympy.pi			# a PyObject with pi
PI = Sym(sympy.pi)		# a Sym object
n(PI, 60)			# 3.14159265358979323846264338327950288419716939937510582097494
```

This is a somewhat awkward way to get `pi` into a symbolic expression. This also works

```
PI = Sym("pi")
```

But this does not `Sym(pi)` (with no quotes, as `Sym` expects character data.) Nor does substitution along the lines of 

```
x = sym"x"
x = subs(x, x, pi)
n(x, 60)			# 3.14159265358979311599796346854418516159057617187500000000000
```

As the value substituted is `julia`'s floating point representation of `pi`, not the symbolic value `sympi.pi`, so gets truncated after enough digits.



### Calculus

* The `limit` function computes simple limits:

```
limit(sin(x  )/x, x, 0)		# 1 as Sym object. Use integer() or float() to convert
limit(sin(y*x)/x, x, 0)		# y
limit(sin(y*x)/x, y, 0) 	# 0
```

One can take limits at infinity using `oo` (but not `Inf`):

```
limit(sin(x)/x, x, Inf)		# ERRORS!
limit(sin(x)/x, x, oo)		# 0
```

* Derivatives, higher-order derivatives and partial derivatives are all computed by `diff`. 

```
diff(sin(x*y), x)		# y⋅cos(x⋅y)         (first derivative, y as constant)
diff(sin(x*y), x, 2)		# -y^2 * sin(x⋅y)    (second derivative in x)
diff(sin(x*y), x, y)		# -x * y * sin(x * y) + cos(x * y)  (mixed partials)
```


* Integration is done through `integrate`. Both definite and indefinite integrals are possible:

```
integrate(x^2, x)		# x^3/3  (no constant term)
integrate(x^2, x, 0, 1)		# 1/3    (catching up to Archimedes)
```


Definite integration can still have parameters involved:

```
(x, a, b) = Sym(:x, :alpha, :beta)
integrate(x^a * (1-x)^b, x, 0, 1) # takes a while, but gives an answer.
```


One could iterate integrations to do double integrals via Fubini's theorem:

```
integrate(integrate(x*y, x, 0, 1), y, 0, 1) # 1/4
```

The calling pattern above is actually a convenience not provided by `SymPy` which uses a tuple to specify the variable to integrate over and the limits:

```
integrate(x^2, (x, 0, 1))	# 1/3
```


For simple integrals this style involves an extra pair of parentheses, but for multiple integrals it proves much more convenient:

```
integrate(x*y, (x, 0, 1), (y, 0, 1)) # still 1/4
```

As well, the inner limits can be expressed using outer variables:
```
integrate(x*y, (x, 0, y), (y, 0,1)) # 1/8
```


* Summations can be done through the `summation` function:

```
summation(1/x, (x, 1, 10))	# 7381 / 2520
summation(1/x, (x, 1, oo))      # zoo (complex infinity, though not sure why here)
summation(1/x^2, (x, 1, oo))	# pi^2/6
```

Taylor Series can be found through the `series` function

```
series(cos(x), x, 0, 3)		# 1 - x^2/2 + O(x^3)  (around 0, of order 3)
```

These could also be generated through the `diff` function:

```
[diff(cos(y), y, i)/factorial(i)*x^i for i in 0:3] | sum | (y == 0) # - x^2/2 + 1
```

* The `solve` function can solve equations.

```
solve(x^2 - 2, x)		# a 2-element Sym Array
```

The output is not simplified. In this case, we can convert to real with `float`:



```
solve(x^2 - 2, x) | float	# [-1.41421, 1.41421]
```

It may turn out to be a bad idea, but for now the `==` operator for `(Sym, Sym)` comparisons is overloaded to `solve`. (Note `Sym` on the right, earlier we mentioned overloading for a number on the right).

```
sin(x) == cos(x)		# [-3*pi/4, pi/4] as Sym array
```







* Differential equations can be solved with `dsolve`. Here we solve *f'(x) + f(x) = 0*. To do so, we need to create a `Function` object. The `SymPy` constructor is call `Function`.  As that keyword is reserved, we call the constructor `SymFunction`:

```
f = SymFunction("f")
eq = diff(f(x), x) + f(x)
dsolve(eq, f(x))		# c1 * exp(-x) as a function object
```	     

Solving *f''(x) + f(x) = 0* is similar, we just take a second derivative

```
eq = diff(f(x), x, 2) + f(x)
dsolve(eq, f(x))		# c1 * sin(x) + c2 * cos(x)
```

### More ...

The `sympy` package has much more functionality than indicated here. For example, there is a lot of functionality related to polynomials and matrices we haven't exposed. For matrices, we can still do some things -- it is just a bit cumbersome. We have a constructor, but none of the methods (though these can easily be added if they seem useful). So to find a determinant of a symbolic matrix can be done, but a bit awkwardly:

```
m = SymMatrix([1 x; x 1])	# a matrix
m[:det]()			# a PyObject with the polynomial -x^2 + 1
Sym(m[:det]())			# make a sym object so that we can call:
solve(Sym(m[:det]()), x)	# an Any Array of PyObjects
map(float, solve(Sym(m[:det]()) , x))      # converts to {-1.0, 1.0}
```

If useful parts of `SymPy` could add to this package, please pass along a request.












