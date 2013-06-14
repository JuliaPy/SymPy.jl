using SymPy
using Base.Test
#### utils


## syms
x = Sym("x")
x = sym"x"
x = Sym(:x)
(x,) = @syms x
x,y = Sym(:x, :y)
x,y = @syms x y
a,b,c = symbols("a b c", commutative=false)

## subs, | (x == number)
f(x) = x^2 - 2
y = f(x)
@assert float(subs(y, x, 1)) == f(1)
@assert float( y | (x == 1) ) == f(1)

y = sym"y"
z = x - 3 + y
subs(z, y, 3)
@assert (z | (x ==2) | (y == 3) | float) == (2 - 3 + 3)

## algebra
expand((x + 1)*(x+2))
x1 = (x + 1)*(x+2)
convert(Sym, x1[:expand]())     #  alternate syntax, perhaps will get easier with pytype_mapping support


## Test conversion of SymPy symbol to Julia expression
x = sym"x"
convert(Expr, simplify(x*(x+1)-x+2))


#### Math
## limits
@assert limit(sin(x)/x, x, 0) | float == 1
(x, h) = @syms x h
out = limit((sin(x+h) - sin(x))/h, h, 0)
@assert (out | (x == pi) | float) == -1.0


## diff
diff(sin(x), x)
out = diff(sin(x), x, 2)
@assert abs((out | (x == pi/4) | float) - - sin(pi/4)) < sqrt(eps())

x,y = @syms x y
diff(x^2 + x*y^2, x, 1)         # partial derivatives


## integrate
integrate(sin(x))
integrate(sin(x), (x, 0, pi))
a,b = @syms a b
integrate(sin(x), (x, a, b))
integrate(sin(x), (x, a, b)) | (a == 0) | (b == pi)


## summation
summation(1/x^2, (x, 1, 10))
@assert convert(Rational, summation(1/x^2, (x, 1, 10)))  == sum([1//x^2 for  x in 1:10])

## matrices
(x,) = @syms x
A = [x 1; 1 x]
a = convert(SymMatrix, A) ## for [:meth] calls
b = [x, 2]
det(A)
a[:det]() | u -> convert(Sym, u)
inv(A)
a[:inv]() | u -> convert(SymMatrix, u) | u -> convert(Array{Sym}, u)
a[:inv]("LU")                   # pass argument to function
SymPy.adjoint(A)
SymPy.dual(A)
SymPy.cholesky(A)

## other functions, could wrap
b = subs(a, x, 2)
map(u -> convert(Sym, u),  b[:QRdecomposition]()) # tuple of matrices

@test a[:is_square] == true
@test a[:is_symmetric]() == true


eigvals(A)


#### math-ops
s = 3
x = Sym("x")
v = [x, 1]
rv = [x 1]
a = [x 1; 1 x]
b = [x 1 2; 1 2 x]

## scalar, [vector, matrix]
@test ( (s + v)  | (x==2) | float .== [3 + 2, 4]) | all
@test ( (v + s)  | (x==2) | float .== [3 + 2, 4]) | all
@test ( (s + rv) | (x==2) | float .== [5 4]) | all
@test ( (s + rv) | (x==2) | float .== [5 4]) | all
@test ( (s + a )  | (x==2) | float .== [2+3 4; 4 2+3]) | all
@test ( (a + a)   | (x==2) | float .== [2+2 1+1; 1+1 2+2]) | all

s - v
v - s
s - rv
rv - s
s - a
a - s

2v   
2rv  
2a   
s * v
v * s
s * rv
rv * s
s * a 
a * s 

s / v  ## broadcasts s
s ./ v 
v / s 
s / rv ## broadcasts s
s ./ rv
rv / s 
s / a ## broadcasts s
s ./ a 
a / s

@test_fails s ^ v ## error
s .^ v  
@test_fails v ^ s ## error
v .^ s 
@test_fails s ^ rv ## error
s .^ rv
@test_fails rv ^ s ## error
rv .^ s 
@test_fails s ^ a ## error
s .^ a
a ^ s
a .^ s 


## vector vector
v + v
@test_fails v + rv ## error
v - v
@test_fails v - rv ## error
@test_fails v * v ## error
v .* v
dot(v, v)
v * rv ## 2x1 1x2 == 2x2
rv * v ## 1x2 2 x 1 == 1x1
v .* rv ## XXX ?? should be what? -- not 2 x 2
rv .* v ## XXX ditto
@test_fails v / v ## error
v ./ v ## ones()
@test_fails v / rv ## error
v ./ rv  ## ??
@test_fails v ^ v ## error
v .^ v
@test_fails v ^ rv ## error
v .^ rv ## ??


## vector matrix
@test_fails v + a ## error (Broadcast?)
@test_fails a + v ## error
v .+ a ## broadcasts
a .+ v
@test_fails v - a ## error
v .- a
@test_fails v * a ## error
v .* a
@test_fails v / a ## error
v ./ a
@test_fails v ^ a ## error
v .^ a

## matrix matrix
a + a
@test_fails a + b ## error
a + 2a
a - a
@test_fails a - b ## error
a * a
a .* a
a * b ## 2x2 * 2*3 -- 2x3
@test_fails a .* b ## error -- wrong size
@test_fails a / a ## error
a ./ a ## ones
@test_fails a / b ## error
@test_fails a ./ b ## error
@test_fails a ^ a ## error
a .^ a
@test_fails a ^ b ## error
@test_fails a .^ b ## error


#### poly.jl

## div
x,y,z = @syms x y z
pf = 5*x^2 + 10*x + 3
pg = 2*x + 2
q, r = div(pf, pg, x, y)


