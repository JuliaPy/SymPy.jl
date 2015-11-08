# SymPy.jl


## About

The `SymPy` package for `julia` brings the symbolic math capabilities
of Python's `sympy` to `julia` users through the `PyCall` package


The `sympy` package (http://sympy.org/) is a Python library for symbolic mathematics. With the excellent `PyCall` package of `julia`, one has access to the many features of `sympy` from a `julia` session.

This `SymPy` package provides a light interface for _some_ of the
features of `SymPy` that makes working with `SymPy` objects a bit
easier.

To use it both `Python` and its `SymPy` package must be installed on
your system. (Install `Python` then download the `sympy` library from
http://code.google.com/p/sympy/downloads/list and install.) [This may
not work on Windows until the zlib issue is resolved.]


## The `PyCall` interface to `SymPy`

The only point to this package is that using `PyCall` to access
`SymPy` is somewhat cumbersome. The following is how one would define
a symbolic value `x`, take its sine, then evaluate at `pi`, say:

```
using PyCall			
@pyimport sympy
#
x = sympy.Symbol("x")
y = sympy.sin(x)
y[:subs](x, pi) |> float
```

The `Symbol` and `sin` function of `SymPy` are found within the
imported `sympy` object. They may be conveniently referenced with
`Python`'s dot notation from the imported `sympy` object, which can be
shortened if desired. However, the `subs` instance method of the `y`
object is accessed differently with `PyCall`: indexing with a
symbol. This returns a function to which we pass the symbolic object
`x` and a value to substitute `pi`.  This leaves the object as a
`PyObject` storing a number which can be brought back into `julia`
through conversion, in this case with the `float` function.

The above isn't so awkward, but  more cumbersome is the similarly
simple task of finding `sin(pi*x)`.  As this multiplication is done at
the python level and is not a method of `sympy` or the `x` object, we
need to evaluate python code. Here is one solution:

```
x = sympy.Symbol("x")
y = pyeval("k*x", k=pi, x=x)    # PyObject 3.14159265358979*x
z = sympy.sin(y)		# PyObject sin(3.14159265358979*x)
z[:subs](x, 1) |> float		# 1.2246467991473532e-16
```

It is possible to simply define operations for `PyObjects`s but
instead, `SymPy` wraps these into a new type, `Sym`.  So, with `SymPy`
this gets replaced by a more `julia`n syntax:

```
using SymPy                     # some warnings need cleaning up
x = sym"x"			# or Sym("x") or Sym(:x) or @syms x
y = sin(pi*x)
subs(y, x, 1) |> float
```

The object `x` we create is of type `Sym`, a simple proxy for the
underlying `PyObject`. The `SymPy` package overloads the familiar math
functions so that working with symbolic expressions can use natural
`julia` idioms.  The `PyCall` interface is always in the
background. To dig the `PyObject` out of a `Sym` object, you access
its property `x`, as in `y.x`.


## Examples


### Making symbolic objects

`SymPy` adds symbolic math to `julia`. Performing symbolic operations
begins by defining simple symbolic objects.  There are many different
ways to make a symbolic object:

```
x = Sym("x")			# Sym constructor
a = Sym(:a)			# using a symbol, not a string
@syms alpha gamma delta # @syms macro
A, B = symbols("A B", commutative=false) # symbols takes assertions as keyword arguments
```

Each new value is a symbolic value.  

### Working with symbolic object

The package overloads many basic math functions to work with this
object type, returning a symbolic object for further manipulations:

```
1/x + 1/x^2 + x			# pretty prints x + 1/x + 1/x^2
```

```
sin(y*x)^2 - cos(y*x)^2
```


## substituting


The `subs` command is used to substitute values. These values are
typically numeric, though they may be other symbols:

``` 
x,y = symbols("x,y")
subs(x + y, x, 3)     # y + 3 
subs(x*y, y, 24 - 2x) # x*(-2*x + 24)
```

Even when the value is numeric, the result of `subs` is a symbolic
object. To bring into `julia`, one must coerce the value with `float`
or `int`, say.

As the role of `subs` is somewhat similar to julia's `replace` function for strings, we overload `replace` for symbolic objects to call `subs` and create a Curried version for use with pipelines:

```
x*y |> replace(y, 3)		# does subs(x*y, y, 3)
```

### Conversion


Basic conversions from `SymPy` numeric types to the corresponding
`julia` objects may be done with the functions `integer`, `float`, and
`complex`. Rational expressions can be converted through
`convert(Rational, ex)`.

```
x |> replace(x, 1) |> int
convert(Rational, sympy.harmonic(30))
```

Sympy has both `N` and `evalf` for conversion. We use `N` to convert to a julia value and `evalf` for numeric conversions within SymPy.


```
evalf(PI) # a Sym object
```

```
N(PI)   # a Float64 value
```

We can pass in an argument to control the number of digits

```
evalf(PI, 60) # still symbolic
```

```
N(PI, 60)  # a BigFloat with precision given by a heuristic
```

### Printing

By default, `SymPy`'s pretty printing is used. This works well enough,
though arrays can get squeezed for space. The `_str` function (not
exported currently) can give a more basic output (from `Python`'s
`__str__` method for objects):

```
x = sym"x"
1/x
[1/x, 1/x^2]
map(SymPy._str,[1/x, 1/x^2])
```

## Mathematics

What does `SymPy` bring to the `julia` user? Basically, the
opportunity to manipulate symbolic expressions. The goal is to provide
a comfortable `julia`-like interface, without worrying about
speed. (There are excess conversions between `PyObject` and `Sym`
objects, ...)


### algebraic expressions

The package provides several functions for working with algebraic expressions.

The `factor` and `expand` functions perform the tasks learned in basic algebra:

```
x,y  = symbols("x, y")
factor(x^2 - 2x + 1)
factor(x^2 - 2x*y + y^2)
expand((x-1)*(x+1))
expand(prod([x-i for i in 1:5]))

```

Rational expressions can be combined with `together` and taken apart through a partial fraction decomposition with `apart`:

```
together(1/x + 1/x^2 + x)	# (x^3 + x + 1) / x^2
```
```
apart(1/(x +2)/(x + 1))		# -1/(x+2) + 1/(x+1)
```

There are a number of different simplification methods: `simplify` is the main one:

```
a = (x + x^2)/(x*sin(y)^2 + x*cos(y)^2)
simplify(a)			# x + 1
```

There are also special purpose simplification functions, such as `trigsimp` (others are listed at http://docs.sympy.org/0.7.2/modules/simplify/simplify.html):

```
trigsimp(a)			# (x^2 + x) / x
```



The `simplify` function has an argument `ratio` to determine how aggressive the simplification should be:

```
root = 1/(sqrt(x) + 3) |> replace(x, 2) # want sqrt(2) to be symbolic
simplify(root, ratio=1)		   # 1/(sqrt(2) + 3)
simplify(root, ratio=oo)	   # -2/sqrt(7) + 3/sqrt(7)
```

The default  is $1.7$.



### Plotting

There is some experimental support for plotting of symbolic expressions. As there are many different backends for plotting with `Julia`, the available features depend on which backend. There is code for the `Plots`, `PyPlot`, `Gadfly`, and `Winston` packages. `PyPlot` provides many more graphics, including 3D graphics.

Basic symbolic expressions can be plotted with `plot`. For example, the following should work with any of the above packages loaded, such as `Gadfly`:

```
using Gadfly
```

```
x = symbols("x")
f(x) = exp(-x) * sin(x)		# a julia function
```

```{asis=true}
plot(f(x), 0, 2pi)	
```


```{asis=true}
plot( [sin(x), diff(sin(x)) ], 0, 2pi) # two plots
```




### Solving equations

The `solve` function provides an interface to solve equations of the type `ex = 0`

```
x,y,a,b,c  = symbols("x y a b c")
solve(x^2 + 3*x + 2, x)
solve(a*x^2 + b*x + c, x)
```



Simultaneous equations can be specified with vector notation:

```
eq1 = x + y - 1
eq2 = x - y - 2
solve([eq1, eq2], [x, y])	# {"x"=>3/2,"y"=>-1/2}
```

The `nsolve` function provides numeric solutions to one or more
equations. Again equations are written as expressions equal to
$0$. Unlike `solve`, the `nsolve` function requires a starting point.

```
nsolve(sin(x) - cos(x), x, 0)
##
x1, x2 = symbols("x1", "x2")
f1 = 3 * x1^2 - 2 * x2^2 - 1
f2 = x1^2 - 2 * x1 + x2^2 + 2 * x2 - 8
nsolve([f1,f2], [x1,x2], [-1,1])
```



### Limits

The `limit` function can be used to compute limits:


```
x, y = symbols("x y")
limit(sin(x)/x, x, 0)		# 1 as Sym object. Use integer() or float() to convert
limit(sin(y*x)/x, x, 0)		# y
limit(sin(y*x)/x, y, 0) 	# 0
```


One can take limits at infinity using `oo` (but not `Inf`):

```
limit(sin(x)/x, x, oo)		# 0
```

One could take derivatives "by hand":

```
x, h = symbols("x h")
f(x) = sin(x)
limit(( f(x+h) - f(x) ) /h, h, 0)
```

Even with more complicated  expressions for the derivative:

```
D(F::Function, x, h) =  (-f(x + 2h) + 8f(x+h) - 8f(x-h) + f(x-2h))/(12h)
limit(D(sin, x, h), h, 0)
```


### Derivatives

The `diff` function is used to compute  derivative of symbolic expressions. One can compute partial derivatives and higher order derivatives:

```
x, y = symbols("x y")
f(x) = exp(-x) * sin(x)
g(x, y) = x^2 + 17*x*y^2
diff(f(x))
diff(f(x), x)			# single variable is redundant
diff(f(x), x, 3)		# f'''(x)
##
diff(sinh(x^2+sqrt(x-1)), x, 3)	# complicated answers
##
diff(g(x,y), x)			# partial in x
diff(g(x,y), x, y)		# mixed partial
```

We can make tangent lines:

```
f(x) = x^x
c = 2
m = diff(f(x), x) |> replace(x, c)
```

```
plot([f(x), f(c) + m*(x-c)], 1, 3)
```

We can solve max and min problems. For example, a normal window with perimeter 20:

```
x,y = symbols("x y")
Area(x, y) = x*y + pi*(x/2)^2/2
y0 = solve(2y + x + pi/2*x - 20, y)[1]
solve(diff(Area(x, y0)), x)
```



### Integration

The `integrate` function provides symbolic integration. (http://docs.sympy.org/0.7.2/modules/integrals/integrals.html)

Indefinite integrals can be computed:

```
x,y,a,b,c = symbols("x y a b c")
integrate(x^2 + x + 2)
integrate(1/((1+x)*(x-1)))
integrate(a*x^2 + b*x + c, x)	# if more than one symbol, specify here
```

Double integrals can be done as well:

```
integrate(x*y, x, y)
```


Definite integrals are specified using tuples:

```
integrate(x^2, (x, 0, 1))
integrate(x*y, (x, 0, 1), (y, 0, 1))
integrate(Sym(1), (x, y, 1), (y, 0, 1)) # 0 <= y <= x <= 1
```

### Taylor series

A Taylor series is an expansion of a function around a point $x_0$, terminating a certain degree, $n$. The `series` function will compute the expansion. One can specify the value of $x_0$ (default is 0$, the degree $n$ (default is $6$). For example, for the function $\sin(x)$, the defaults produce:

```
series(sin(x), x)			# uses x0=0, n=6
```

To find an expansion around another point, say $pi/4$, we would have:

```
series(sin(x), x, pi/4)
```

To get more terms, say up to degree 10, could be done with:

```
series(sin(x), x, pi/4, 10)
```

### Series

Sympy has some functions for dealing with sums, finite and infinite. The `Sum` function is used in a similar manner as `integrate`. Here we find the sum of $1/i^2$:

```
i, n = symbols("i, n")
Sum(1/i^2, (i, 1, oo))
Sum(1/i^2, (i, 1, oo)) |> doit
```

Finite sums are also computable using rational math:

```
Sum(1/i, (i, 1, 100)) |> doit
```

## Matrices

The sympy package has a matrix class that can treat matrices as
symbolic objects. `SymPy` uses the `SymMatrix` type as a proxy for
this class. However, most matrices are converted into `julia` arrays
of symbolic objects (`Array{Sym}`). This allows `julia` to handle many
of the common matrix operations, such as addition, subtraction ... It
also means there are extra conversions going on that may slow things
down a bit. It is hoped this tradeoff is worth it.

### Construction

Matrices can be constructed as usual just by using a symbolic object:

```
x, y = symbols("x y")
m = [x 1; 1 x]
```

The basic matrix arithmetic should work:

```
m * m
m .* m
```

Indexing should be fine:
```
m[1,2] = 2
```


Some basic functions work directly with the symbolic values:

```
trace(m)
det(m)
inv(m)
eigvals(m)
eigvecs(m)
dot(m, [1, x])
```


There are many other matrix functions provided by SymPy. Many are provided as methods. For example:

```
jordan_form(m)
```


