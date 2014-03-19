## Package to bring `sympy` functionality into `julia` via `PyCall`

The `SymPy` package  (`http://sympy.org/`)  is a Python library for symbolic mathematics. 

With the excellent `PyCall` package of `julia`, one has access to the
many features of `SymPy` from a `julia` session.

This `SymPy` package provides a light interface for _some_ of the
features of `SymPy` that makes working with `SymPy` objects a bit
easier.

To use it, both `Python` and the `SymPy` package must be installed on
your system. (Install `Python` then download the `sympy` library from
http://code.google.com/p/sympy/downloads/list and install.)

## The `PyCall` interface to `SymPy`

The only point to this package is that using `PyCall` to access
`SymPy` is somewhat cumbersome. The following is how one would define
a symbolic value `x`, take its sine, then evaluate at `pi`, say:

```
using PyCall			
@pyimport sympy
x = sympy.Symbol("x")
y = sympy.sin(x)
y[:subs](x, pi) |> float
```

The `Symbol` and `sin` function of `SymPy` are found within the
imported `sympy` object. They may be referenced with `Python`'s dot
notation. However, the `subs` method of the `y` object is accessed
differently, using indexing notation with a symbol. The call above
substitutes a value of `pi` for `x`. This leaves the object as a
`PyObject` storing a number which can be brought back into `julia`
through conversion, in this case with the `float` function.

The above isn't so awkward, but even more cumbersome is the similarly
simple task of finding `sin(pi*x)`.  As this multiplication is done at
the python level and is not a method of `sympy` or the `x` object, we
need to evaluate python code. Here is one solution:

```
x = sympy.Symbol("x")
y = pyeval("k*x", k=pi, =x)     # PyObject 3.14159265358979*x
z = sympy.sin(y)		# PyObject sin(3.14159265358979*x)
z[:subs](x, 1) |> float		# 1.2246467991473532e-16
```

This gets replaced by a more `julia`n syntax:

```
using SymPy                     # some warnings need cleaning up
x = sym"x"			# or Sym("x") or Sym(:x) or (x,) = @syms x
y = sin(pi*x)
subs(y, x, 1) |> float
```

The object `x` we create is of type `Sym`, a simple proxy for the
underlying `PyObject`. We then overload the familiar math functions so
that working with symbolic expressions can use natural `julia` idioms.

However, the `PyCall` interface is needed for serious work, as only a
small portion of the `SymPy` interface is exposed.  To dig the
`PyObject` out of a `Sym` object, you access its property `x`, as in
`y.x`. This is useful if passing a `Sym` object to a method call,
though `getindex` is overridden for `Sym` objects and symbol indices
to call the method.

## Examples

To make a symbolic object (of type `Sym`) we have the `Sym`
constructor, the convenient `sym` macro, and `@syms`

```
x = Sym("x")
h, y = Sym("h", :y)
a, b, c = @syms a b c
```

Operator overloading of the basic math functions allows symbolic
expressions to be combined without fuss:

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
subs(x + y, x, 3)     # y + 3
subs(x*y, y, 24 - 2x) # x*(-2*x + 24)
```

The `subs` function is similar in spirit to `replace`, so we overload
`replace` for symbolic expressions and create a Curried version for
use in pipelines:

```
x*y |> replace(y, 3)		# 3x
```

(This had been `==(x::Sym, y::Number)` but `==` is now used with `solve`.)

### Printing

By default, `SymPy`'s pretty printing is used. This works well enough,
though arrays can get squeezed for space. The `_str` function can give
a more basic output (from `Python`'s `__str__` method for objects):

```
julia> 1/x
1
─
x

julia> [1/x, 1/x^2]
2-element Sym Array
⎡1 ⎤
⎢─ ⎥
⎢x ⎥
⎢  ⎥
⎢1 ⎥
⎢──⎥
⎢ 2⎥
⎣x ⎦

julia> _str([1/x, 1/x^2])
2-element ASCIIString Array:
 "1/x"    
 "x**(-2)"
```

### Plotting

The plotting of symbolic expressions is made convenient through their coercion to functions. 
The basic call `convert(Function, symbolic_expression)` produces a function, but the values are symbolic. To plot a function, say with `Winston`, we have:

```
using SymPy, Winston
x = sym"x"
f1 = convert(Function, x^2 - 2x + 1)
plot(x -> float(f1(x)), 0, 3)
```

We can encapsulate this into a function call with:

```
function plt(ex::SymPy.Sym, args...)
   f = x -> float(convert(Function, ex)(x))
   plot(f, args...)
end
```


## Conversion

The key to plotting working is forming a function object from the `Sym` object.
Here is one pattern to do so where we need to be careful to match the
variable we substitute with (`x`) with the expression.

```
u -> float(subs( exp(-x) * sin(x), x, u)) # anonymous function to evaluate expression
```

This is basically what happens in the call: `convert(Function, exp(-x)*sin(x))`, 
though that call replaces a lone free variable, not
necessarily one named `x`. This conversion can be used to plot
expressions with `julia`'s other plotting packages.

Basic conversions from `SymPy` numeric types to the corresponding
`julia` objects may be done with the functions `integer`, `float`, and
`complex`. Rational expressions can be converted through
`convert(Rational, ex )`.


The `sympy` function `n` can be used to form a numerical value from an
expression, though the expression is still a `Sym` instance.

```
sqrt(x + 1) |> replace(x, 2)       # pretty print sqrt(3)
sqrt(x + 1) |> replace(x, 2) |> n	 # 1.73205080756888 as Sym object
```




The `n` function takes an argument to control the number of
digits. For example, we can do

```
sympy.pi			# a PyObject with pi
PI = Sym(sympy.pi)		# a Sym object
n(PI, 60)			# 3.14159265358979323846264338327950288419716939937510582097494
```

This is a somewhat awkward way to get `pi` into a symbolic expression. This also works

```
PI = Sym("pi")
```

A substitution along the
lines of

```
x = sym"x"
x = subs(x, x, pi)
n(x, 60)			# 3.14159265358979311599796346854418516159057617187500000000000
```

loses precision, as the value substituted is `julia`'s floating point
representation of `pi`, not the symbolic value `sympi.pi`, so gets
truncated after enough digits.



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

The `limit` function performs better on some functions than simply
trying to explore limits numerically. The functionality is based on Gruntz's
algorithm. For example, numerically it appears that this limit is $0$:

```
f(x) = x^(1 - log(log(log(log(1/x)))))
x1 = 10.0.^(-[5:16])
[x1 map(f, x1)]
```

But in fact this limit blows up:

```
limit(f(x), x, 0)
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

The calling pattern above is actually a convenience not provided by
`SymPy` which uses a tuple to specify the variable to integrate over
and the limits:

```
integrate(x^2, (x, 0, 1))	# 1/3
```


For simple integrals this style involves an extra pair of parentheses,
but for multiple integrals it proves much more convenient:

```
integrate(x*y, (x, 0, 1), (y, 0, 1)) # still 1/4
```

As well, the inner limits can be expressed using outer variables: ```
integrate(x*y, (x, 0, y), (y, 0,1)) # 1/8 ```


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
[diff(cos(y), y, i)/factorial(i)*x^i for i in 0:3] |> sum |> replace(y, 0) # - x^2/2 + 1
```

* The `solve` function can solve equations.

```
solve(x^2 - 2, x)		# a 2-element Sym Array
```

The output is not simplified. In this case, we can convert to real with `float`:



```
solve(x^2 - 2, x) |> float	# [-1.41421, 1.41421]
```




* Differential equations can be solved with `dsolve`. Here we solve
  *f'(x) + f(x) = 0*. To do so, we need to create a `Function`
  object. The `SymPy` constructor is call `Function`.  As that keyword
  is reserved, we call the constructor `SymFunction`:

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

### Vectors and matrices

There are some conveniences for working with symbolic vectors and matrices. First, defining them is easy enough:

```
v = [x, 1]
A = [x 1; 1 -x]
```

Basic math operations should match those of `julia` (though there could be mistakes!)

```
v .* v				# 2 element array
dot(v, v)			# not quite v' * v
A * v				# 2 element array
v * A				# error on shape
A^2				
A.^2				#
```

These are arrays of symbolic objects. Some functions are defined for such:

```
det(A)				# determinant
inv(A)				# inverse
exp(A)				# matrix exponential, many screenfuls
trace(A)
eigvals(A)
eigvecs(A)
rref(A)				# reduced row echelon form
```

The SymPy matrix instances have methods. To convert an array of `Sym`s into a `Sym` array, can be done as follows:

```
a = convert(SymMatrix, A)
```

Then methods can be called using `[:symbol]`:

```
a[:is_symmetric]()
a[:is_square]			# is_square not function call.
P, J = a[:jordan_form]()	# returns a tuple
convert(Array{Sym}, convert(SymMatrix, J)) # need to work to get out components
```


### More ...

The `sympy` package has much more functionality than indicated
here. For example, there is a lot of functionality related to
polynomials we haven't exposed.

If useful parts of `SymPy` could add to this package, please pass
along a request.

The `call_meth` function can make it easy to bring in sympy functions to `julia`. For example, to define a `cancel` function which in sympy has signature `(ex, extension=x)` can be done with:

```
Kancel(args...; kwargs...) = call_meth(:cancel, args...; kwargs...) ## already have cancel
sq2 = (sqrt(x) |> replace(x, 2))
f = x^3 + (sq2- 2)*x^2 - (2*sq2 + 3)*x - 3*sq2
g = x^2 - 2
Kancel(f/g)
Kancel(f/g, extension=true)  
```

For functions without keyword arguments, it is even easier:

```
harmonic(n::Integer) = call_meth(:harmonic, n)
harmonic(30)
convert(Rational, harmonic(30))
```

Some conversions from `PyObject` to `Sym` are not automatic. In that case, use `sympy_meth` and work with the `PyObject`.









