using SymPy
using Base.Test

## syms
x = Sym("x")
#x = sym"x" # deprecated
x = Sym(:x)
(x,) = @syms x
x,y = Sym(:x, :y)
x,y = @syms x y
x,y = symbols("x,y")

## number conversions
Sym(2)
Sym(2.0)
Sym(big(2))
Sym(big(2.0))                   # may need mpmath (e.g., conda install mpmath)
Sym(2//1)
Sym(im)
Sym(2im)
Sym(1 + 2im)
Sym(pi)
Sym(e)
Sym(catalan)
            
      


## subs, |> (x == number)
f(x) = x^2 - 2
y = f(x)
@assert float(subs(y, x, 1)) == f(1)
@assert float( y |> replace(x,1) ) == f(1)

y = Sym("y")
z = x - 3 + y
subs(z, y, 3)
@assert (z |> replace(x, 2) |> replace(y, 3) |> float) == (2 - 3 + 3)

## Conversion
x = Sym("x")
p = subs(x,x,pi)
q = subs(x,x,1//2)
r = subs(x,x,1.2)
z = subs(x,x,1)
@assert isa(N(p), Float64)
@assert isa(N(p, 60), BigFloat)
@assert isa(evalf(p), Sym)
@assert isa(N(q), Rational)
@assert isa(N(r), Float64)
@assert isa(N(z), Int)

## algebra
expand((x + 1)*(x+2))
x1 = (x + 1)*(x+2)
expand(x1)
expand_trig(sin(2x))


## limits
@assert limit(sin(x)/x, x, 0) |> float == 1
(x, h) = @syms x h
out = limit((sin(x+h) - sin(x))/h, h, 0)
@assert (out |> replace(x, pi) |> float) == -1.0


## diff
diff(sin(x), x)
out = diff(sin(x), x, 2)
@assert abs((out |> replace(x, pi/4) |> float) - - sin(pi/4)) < sqrt(eps())

x,y = @syms x y
diff(x^2 + x*y^2, x, 1)         # partial derivatives

t = symbols("t", real=true)     # vector-valued functions
r1(t) = [sin(t), cos(t), t]
u = r1(t)
kappa = norm(diff(u) × diff(u,t,2)) / norm(diff(u))^3 |> simplify
@assert convert(Rational,kappa) == 1//2



## integrate
integrate(sin(x))
integrate(sin(x), (x, 0, pi))
a,b = @syms a b
integrate(sin(x), (x, a, b))
integrate(sin(x), (x, a, b)) |> replace(a, 0) |> replace(b, pi)


## summation
summation(1/x^2, (x, 1, 10))
out = summation(1/x^2, (x, 1, 10))
out1 = sum([1//x^2 for  x in 1:10])
@assert out[:p] |> integer == out1.num
@assert out[:q] |> integer == out1.den

## matrices
(x,) = @syms x
A = [x 1; 1 x]
a = convert(SymMatrix, A) ## for [:meth] calls
b = [x, 2]
det(A)
det(a)

## we use inverse for A[:inv]()
inv(A) # aliased to use inverse
inverse(A)
a[:inv]() |> u -> convert(Array{Sym}, u)
a[:inv]("LU")                   # pass argument to function
adjoint(A)
dual(A)
cholesky(A)
## other functions, could wrap
b = subs(a, x, 2)
QRdecomposition(b)

@assert is_square(a) == true
@assert is_symmetric(a) == true


eigvals(A)

a = [1 0 0; 0 1 0; 0 0 x]
evecs = eigvecs(a)
@assert float(evecs) == eye(3)


## Ops
s = 3
x = Sym("x")
v = [x, 1]
rv = [x 1]
a = [x 1; 1 x]
b = [x 1 2; 1 2 x]

const DIMERROR = VERSION < v"0.4.0-dev" ? ErrorException : DimensionMismatch

## scalar, [vector, matrix]
s + v
v + s
s + rv
rv + s
s + a
a + s

s - v
v - s
s - rv
rv - s
s - a
a - s

2v   
2rv  
2a   
s .* v
v .* s
s .* rv
rv .* s
s .* a 
a .* s 

## s / v  ## broadcasts s Depreacated
s ./ v 
v / s
v .\ s
s \ v
## s / rv ## broadcasts s Deprecated
s ./ rv
rv / s
rv .\ s
s \ rv
## s / a ## broadcasts s Deprecated
s ./ a 
a / s
a .\ s
s \ a

@test_throws MethodError  s ^ v ## error
s .^ v  
@test_throws MethodError  v ^ s ## error
v .^ s 
@test_throws MethodError  s ^ rv ## error
s .^ rv
@test_throws DimensionMismatch  rv ^ s ## error
rv .^ s 
@test_throws MethodError  s ^ a ## error
s .^ a
a ^ s
a .^ s 


## vector vector
v .+ v
##@test_throws MethodError  v .+ rv ##  no longer an error, broadcase
v .- v
@test_throws DIMERROR  v - rv
@test_throws MethodError  v * v ## error
v .* v
dot(v, v)
v * rv ## 2x1 1x2 == 2x2
rv * v ## 1x2 2 x 1 == 1x1
v .* rv ## XXX ?? should be what? -- not 2 x 2
rv .* v ## XXX ditto
@test_throws MethodError  v / v ## error
v ./ v ## ones()
v .\ v
@test_throws MethodError  v / rv ## error
v ./ rv  ## ??
rv .\ v
@test_throws MethodError  v ^ v ## error
v .^ v
@test_throws MethodError  v ^ rv ## error
v .^ rv ## ??


## vector matrix
@test_throws DIMERROR v + a ## error (Broadcast?)
@test_throws DIMERROR a + v ## error
v .+ a ## broadcasts
a .+ v
@test_throws DIMERROR  v - a ## error
v .- a
@test_throws DimensionMismatch  v * a ## error
v .* a
#@test_throws MethodError  v / a ## error
v ./ a
a .\ v
@test_throws MethodError  v ^ a ## error
v .^ a
v
## matrix matrix
a + a
@test_throws DIMERROR  a + b ## error
a + 2a
a - a
@test_throws DIMERROR  a - b ## error
a * a
a .* a
a * b ## 2x2 * 2*3 -- 2x3
@test_throws DIMERROR  a .* b ## error -- wrong size
#@test_throws MethodError  a / a
a ./ a ## ones
a .\ a
@test_throws MethodError  a / b ## error
@test_throws DIMERROR  a ./ b ## error
@test_throws DIMERROR  b .\ a ## error
@test_throws MethodError  a ^ a ## error
a .^ a
@test_throws MethodError  a ^ b ## error
@test_throws DIMERROR  a .^ b ## error


## Number theory
@test isprime(100) == isprime(Sym(100))
@test prime(Sym(100)) == 541
@test int(collect(primerange(Sym(2),10))) == primes(10)
@test multiplicity(Sym(10), 100) == 2
@test factorint(Sym(100)) == factor(100)

## polynomials
x,y = @syms x y
f1 = 5x^2  + 10x + 3
g1 = 2x + 2
q,r = polydiv(f1,g1, domain="QQ") # not div, as can't disambiguate div(Sym(7), 5)) to do integer division
@test r == Sym(-2)
@test simplify(q*g1 + r - f1) == Sym(0)

## ODEs
x, a = Sym("x, a")
F = symbols("F", cls=SymFunction)
ex = Eq(diff(F(x),x), a*F(x))
ex1 = dsolve(ex, F(x))
ex2 = rhs(ex1) |> subs(Sym(:C1), 1) |> subs(a, 2)
@assert ex2 == exp(2x)

## piecewise
x = Sym("x")
p = piecewise((x, Ge(x,0)), (0, Lt(x,0)), (1, Eq(x,0)))
## using infix \ll<tab>, \gt<tab>, \Equal<tab>
p = piecewise((x, x ≥ 0), (0, x ≪ 0), (1, x ⩵ 0))
@assert int(subs(p,x,2)) == 2
@assert int(subs(p,x,-1)) == 0
@assert int(subs(p,x,0)) == 0

## More logical expressions
(x ≪ 0) ∧ (x*y ≤ 1) ∨ (x ⩵ y) ∧ (¬(x ≫ 3))


## relations
x,y=symbols("x, y")
ex = Eq(x^2, x)
@assert lhs(ex) == x^2
@assert rhs(ex) == x
@assert args(ex) == (x^2, x)

## mpmath functions
x = Sym("x")
@assert limit(besselj(1,1/x), x, 0) == Sym(0)
complex(hankel2(2, pi))
bei(2, 3.5)
bei(1+im, 2+3im)
