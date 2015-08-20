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
