using SymPy
using Base.Test
using Compat

## Symbol creation
x = Sym("x")
#x = sym"x" # deprecated
x = Sym(:x)
x,y = Sym(:x, :y)
x,y = symbols("x,y")

@syms u1 u2 u3
@syms u positive=true
@test length(solve(u+1)) == 0
# make sure @syms defines in a local scope
let
    @syms w
end
@test_throws UndefVarError isdefined(w)


## extract symbols
ex = x*y
@test isa(free_symbols(ex), Vector{Sym})

## number conversions
Sym(2)
Sym(2.0)
Sym(2//1)
Sym(im)
Sym(2im)
Sym(1 + 2im)
Sym(pi)
Sym(e)
Sym(catalan)

## function conversion
f1 = convert(Function, x^2)
@test f1(2) == Sym(4)

### Subs
## subs, |> (x == number)
f(x) = x^2 - 2
y = f(x)
@assert float(subs(y, x, 1)) == f(1)
@assert float( y |> subs(x,1) ) == f(1)

## interfaces
ex = (x-1)*(y-2)
@assert subs(ex, x, 1) == 0
@assert subs(ex, (x,1)) == 0
@assert subs(ex, (x,2),(y,2)) == 0

if VERSION >= v"0.4.0"
    @assert subs(ex, x=>1) == 0
    @assert subs(ex, x=>2, y=>2) == 0
    @assert subs(ex, Dict(x=>1)) == 0
    @assert ex(x=>1) == 0
end

## match, replace, xreplace, rewrite
x,y,z = symbols("x, y, z")
a,b,c = map(Wild, (:a,:b,:c))
## match: we have pattern, expression to follow `match`
d = match(a^a, (x+y)^(x+y))
@test d[a] == x+y
d = match(a^b, (x+y)^(x+y))
@test d[b] == x + y
ex = (2x)^2
pat = a*b^c
d = match(pat, ex)
@test d[a] == 4 && d[b] == x && d[c] == 2
@test xreplace(pat, d) == 4x^2

## replace
if VERSION >= v"0.4.0"
ex = log(sin(x)) + tan(sin(x^2))
@test replace(ex, func(sin(x)), func(cos(x))) == log(cos(x)) + tan(cos(x^2))
#XXX@test replace(ex, func(sin(x)), u ->  sin(2u)) == log(sin(2x)) + tan(sin(2*x^2))
@test replace(ex, sin(a), tan(a)) ==  log(tan(x)) + tan(tan(x^2))
@test replace(ex, sin(a), a) == log(x) + tan(x^2)
@test replace(x*y, a*x, a) == y
end

## xreplace
if VERSION >= v"0.4.0"            
@test xreplace(1 + x*y, x => PI) == 1 + PI*y
@test xreplace(x*y + z, x*y => PI) == z + PI
@test xreplace(x*y * z, x*y => PI) == x* y * z
@test xreplace(x +2 + exp(x + 2), x+2=>y) == x + exp(y) + 2
end
            

#Test subs for pars and dicts
ex = 1
dict1 = Dict{Compat.String,Any}()
dict2 = Dict{Any,Any}()
#test subs
for i=1:4
    x = Sym("x$i")
    ex=ex*x
    dict2[x] = i
    dict1[string(x)] = i
end
for d in [dict1, dict2]
    @test ex |> subs(d) == factorial(4) 
    @test subs(ex, d) == factorial(4)
    @test subs(ex, d...) == factorial(4)
    if VERSION >= v"0.4.0" 
        @test ex |> subs(d...) == factorial(4)
        @test ex(d) == factorial(4)
        @test ex(d...) == factorial(4)
    end
end

a = Sym("a")
b = Sym("b")
line = x -> a + b * x
sol = solve([line(0)-1, line(1)-2],[a,b])
ex = line(10)
@test ex |> subs(sol) == 11
if VERSION >= v"0.4.0" 
    @test ex(sol) == ex(sol...) == 11
end

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
@assert isa(N(z), Integer)

## method calls via getindex
p = (x-1)*(x-2)
p[:roots]()  # sympy.roots
p = Poly(p, x)
p[:coeffs]() # p.coeffs

## algebra
expand((x + 1)*(x+2))
x1 = (x + 1)*(x+2)
expand(x1)
expand_trig(sin(2x))

## math functions
u = abs(x^2 - 2)
@test u(x=>0) == 2
u = min(x, x^2, x^3, x^4)
@test u(x=>2) == 2
@test u(x=>1//2) == 1//2^4

## solve
x,y,a = symbols("x,y,a", real=true)
solve(x^2 - 2x)
solve(x^2 - 2a, x)
solve(x^2 - 2a, a)
solve(Lt(x-2, 0))
solve( x-2 ≪ 0)
exs = [x-y-1, x+y-2]
d = solve(exs)
@assert d[x] == 3//2
@assert map(ex -> subs(ex, d), exs) == [0,0]
solve([x-y-a, x+y], [x,y])



## limits
@assert limit(sin(x)/x, x, 0) |> float == 1
(x, h) = @syms x h
out = limit((sin(x+h) - sin(x))/h, h, 0)
@assert (out |> replace(x, pi) |> float) == -1.0


## diff
diff(sin(x), x)
out = diff(sin(x), x, 2)
@assert abs((out |> replace(x, pi/4) |> float) - - sin(pi/4)) < sqrt(eps())

@syms x y
diff(x^2 + x*y^2, x, 1)         # partial derivatives

t = symbols("t", real=true)     # vector-valued functions
r1(t) = [sin(t), cos(t), t]
u = r1(t)
#kappa = norm(diff(u) × diff(u,t,2)) / norm(diff(u))^3 |> simplify
#@assert convert(Rational,kappa) == 1//2

u = SymFunction("u")
eqn = Eq(x^2 + u(x)^2, x^3 - u(x))
diff(eqn, x)

## integrate
integrate(sin(x))
integrate(sin(x), (x, 0, pi))
#a, b, t = symbols("a, b, t", real=true)
a, b, t = symbols("a, b, t")
integrate(sin(x), (x, a, b))
integrate(sin(x), (x, a, b)) |> replace(a, 0) |> replace(b, pi)
integrate(sin(x) * DiracDelta(Sym(0))) # sin(0)
integrate(Heaviside(x), (x, -1, 1))
C = Curve([exp(t)-1, exp(t)+1], (t, 0, log(Sym(2))))
line_integrate(x + y, C, [x,y])



## summation
summation(1/x^2, (x, 1, 10))
out = summation(1/x^2, (x, 1, 10))
out1 = sum([1//x^2 for  x in 1:10])
@assert @compat round(Integer, out.x[:p]) == out1.num
@assert @compat round(Integer, out.x[:q]) == out1.den

## matrices
## these can also be tested by matrix-tests.jl
@syms x
A = [x 1; 1 x]
b = [x, 2]



## Ops
s = 3
x = Sym("x")
v = [x, 1]
rv = [x 1]
a = [x 1; 1 x]
b = [x 1 2; 1 2 x]

const DIMERROR = VERSION < v"0.4.0-dev" ? ErrorException : DimensionMismatch
const DimensionOrMethodError =  Union{MethodError, DimensionMismatch}  
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
@test_throws DimensionOrMethodError v ^ s ## error
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
@test_throws DimensionOrMethodError   v * v ## error
v .* v
dot(v, v)
if VERSION >= v"0.6.0-dev"
    @test_throws DimensionMismatch v * rv ## 2x1 1x2 == 2x2
else
    v * rv
end
rv * v ## 1x2 2 x 1 == 1x1
v .* rv ## XXX ?? should be what? -- not 2 x 2
rv .* v ## XXX ditto
## @test_throws MethodError  v / v ## error
v ./ v ## ones()
v .\ v
## @test_throws MethodError  v / rv ## error
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
##@test_throws MethodError  a / b ## error
@test_throws DIMERROR  a ./ b ## error
@test_throws DIMERROR  b .\ a ## error
@test_throws MethodError  a ^ a ## error
a .^ a
@test_throws MethodError  a ^ b ## error
@test_throws DIMERROR  a .^ b ## error


## Number theory
#@test isprime(100) == isprime(Sym(100))
#@test factorint(Sym(100)) == factor(100)
@test prime(Sym(100)) == 541
@test multiplicity(Sym(10), 100) == 2


## polynomials
@syms x y
f1 = 5x^2  + 10x + 3
g1 = 2x + 2
q,r = polydiv(f1,g1, domain="QQ") # not div, as can't disambiguate div(Sym(7), 5)) to do integer division
@test r == Sym(-2)
@test simplify(q*g1 + r - f1) == Sym(0)


## piecewise
x = Sym("x")
p = piecewise((x, Ge(x,0)), (0, Lt(x,0)), (1, Eq(x,0)))
## using infix \ll<tab>, \gt<tab>, \Equal<tab>
p = piecewise((x, (x ≫ 0)), (0, x ≪ 0), (1, x ⩵ 0))
@assert subs(p,x,2) == 2
@assert subs(p,x,-1) == 0
@assert subs(p,x,0) == 1

u = ifelse(Lt(x, 0), "neg", ifelse(Gt(x, 0), "pos", "zero"))
@assert subs(u,x,-1) == Sym("neg")
@assert subs(u,x, 0) == Sym("zero")
@assert subs(u,x, 1) == Sym("pos")

p = piecewise((-x, x ≪ 0), (x, x ≧ 0))


## relations
x,y=symbols("x, y")
ex = Eq(x^2, x)
@assert lhs(ex) == x^2
@assert rhs(ex) == x
@assert args(ex) == (x^2, x)

## mpmath functions
if isdefined(:mpmath)
    x = Sym("x")
    Sym(big(2))
    Sym(big(2.0))                   # may need mpmath (e.g., conda install mpmath)
    
    @assert limit(besselj(1,1/x), x, 0) == Sym(0)
    complex(hankel2(2, pi))
    bei(2, 3.5)
    bei(1+im, 2+3im)
end

## Assumptions
@assert ask(Q.even(Sym(2))) == true
@assert ask(Q.even(Sym(3))) == false
@assert ask(Q.nonzero(Sym(3))) == true

## sets
s = FiniteSet("H","T")
s1 = powerset(s)
VERSION >= v"0.4.0" && @assert length(collect(convert(Set, s1))) == length(collect(s1.x))
a, b = Interval(0,1), Interval(2,3)
@assert is_disjoint(a, b) == true
@assert measure(union(a, b)) == 2


## Issue # 56
@assert Sym(1+2im) == 1+2IM


## Issue #59
cse(sin(x)+sin(x)*cos(x))
cse([sin(x), sin(x)*cos(x)])
cse([sin(x) sin(x)*cos(x); cos(x) sin(x)*cos(x)])

## Issue #60, lambidfy
if VERSION >= v"0.4.0"
    #@vars x,y
    x, y = symbols("x, y")
    lambdify(sin(x)*cos(2x) * exp(x^2/2))
    fn = lambdify(sin(x)*asin(x)*sinh(x)); fn(0.25)
    lambdify(real(x)*imag(x))
#    @assert lambdify(Min(x,y))(3,2) == 2
    
    ex = 2*x^2/(3-x)*exp(x)*sin(x)*sind(x)
    fn = lambdify(ex); map(fn, rand(10))
    ex = x - y
    #@assert lambdify(ex)(3,2) == 1

    i = Indicator(x, 0, 1)
    u = lambdify(i)
    @assert u(.5) == 1
    @assert u(1.5) == 0
end

## issue #67
@assert N(Sym(4//3)) == 4//3

## issue #71
@test log(Sym(3), Sym(4)) == log(Sym(4)) / log(Sym(3))

## issue #103 # this does not work for `x` (which has `classname(x) == "Symbol"`), but should work for other expressions
@vars x y z
for ex in (sin(x), x*y^2*x, sqrt(x^2 - 2y))
    @test func(ex)(args(ex)...) == ex
end
