using SymPy
using Base.Test

## syms
x = Sym("x")
x = sym"x"
x = Sym(:x)
(x,) = @syms x
x,y = Sym(:x, :y)
x,y = @syms x y

## subs, |> (x == number)
f(x) = x^2 - 2
y = f(x)
@assert float(subs(y, x, 1)) == f(1)
@assert float( y |> replace(x,1) ) == f(1)

y = sym"y"
z = x - 3 + y
subs(z, y, 3)
@assert (z |> replace(x, 2) |> replace(y, 3) |> float) == (2 - 3 + 3)

## algebra
expand((x + 1)*(x+2))
x1 = (x + 1)*(x+2)
expand(x1)




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


## Ops
s = 3
x = Sym("x")
v = [x, 1]
rv = [x 1]
a = [x 1; 1 x]
b = [x 1 2; 1 2 x]

## scalar, [vector, matrix]
s .+ v
v .+ s
s .+ rv
rv .+ s
s .+ a
a .+ s

s .- v
v .- s
s .- rv
rv .- s
s .- a
a .- s

2v   
2rv  
2a   
s .* v
v .* s
s .* rv
rv .* s
s .* a 
a .* s 

s / v  ## broadcasts s
s ./ v 
v / s 
s / rv ## broadcasts s
s ./ rv
rv / s 
s / a ## broadcasts s
s ./ a 
a / s

@test_throws s ^ v ## error
s .^ v  
@test_throws v ^ s ## error
v .^ s 
@test_throws s ^ rv ## error
s .^ rv
@test_throws rv ^ s ## error
rv .^ s 
@test_throws s ^ a ## error
s .^ a
a ^ s
a .^ s 


## vector vector
v + v
@test_throws v + rv ## error
v - v
@test_throws v - rv ## error
@test_throws v * v ## error
v .* v
dot(v, v)
v * rv ## 2x1 1x2 == 2x2
rv * v ## 1x2 2 x 1 == 1x1
v .* rv ## XXX ?? should be what? -- not 2 x 2
rv .* v ## XXX ditto
@test_throws v / v ## error
v ./ v ## ones()
@test_throws v / rv ## error
v ./ rv  ## ??
@test_throws v ^ v ## error
v .^ v
@test_throws v ^ rv ## error
v .^ rv ## ??


## vector matrix
@test_throws v + a ## error (Broadcast?)
@test_throws a + v ## error
v .+ a ## broadcasts
a .+ v
@test_throws v - a ## error
v .- a
@test_throws v * a ## error
v .* a
@test_throws v / a ## error
v ./ a
@test_throws v ^ a ## error
v .^ a

## matrix matrix
a + a
@test_throws a + b ## error
a + 2a
a - a
@test_throws a - b ## error
a * a
a .* a
a * b ## 2x2 * 2*3 -- 2x3
@test_throws a .* b ## error -- wrong size
@test_throws a / a ## error
a ./ a ## ones
@test_throws a / b ## error
@test_throws a ./ b ## error
@test_throws a ^ a ## error
a .^ a
@test_throws a ^ b ## error
@test_throws a .^ b ## error
