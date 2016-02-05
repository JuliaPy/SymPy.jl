using SymPy
using Base.Test



## ODEs
x, a = Sym("x, a")
F = symbols("F", cls=SymFunction)
ex = Eq(diff(F(x),x), a*F(x))
ex1 = dsolve(ex, F(x))
ex2 = rhs(ex1) |> subs(Sym(:C1), 1) |> subs(a, 2)
@assert ex2 == exp(2x)

t, = @syms t
X, Y = symbols("X, Y", cls=SymFunction)
eq = [Eq(diff(X(t),t), 12*t*X(t) + 8*Y(t)), Eq(diff(Y(t),t), 21*X(t) + 7*t*Y(t))]
dsolve(eq)


if VERSION >= v0.4.0
    u = IVPSolution("u")
    a,x, y, y0, y1 = symbols("a, x, y, y0, y1")

    ivpsolve(u'(x) - a*u(x), x, (u, 0, 1))
    ivpsolve(u'(x) - a*u(x), x, (u, 0, y1))
    ivpsolve(u'(x) - a*u(x), x, (u, y0, y1))
    ivpsolve(x*u'(x) + x*u(x) + 1, x, (u, 1, 1))
    ivpsolve((u'(x))^2 - a*u(x), x, (u, 0, 1))
    ivpsolve(u''(x) - a * u(x), x, (u, 0, 1), (u', 0, 0))

    f, g, k = map(SymFunction, ["f", "g", "k"])
    eqn = f(x)*u'(y)*y + g(x)*u(y) + k(x)
    ivpsolve(eqn, y, (u, 1, 0))
end
