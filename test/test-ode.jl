using SymPy
using Test

@testset "ODes" begin
    ## ODEs
    x, a = Sym("x, a")
    F = SymFunction("F")
    ex = diff(F(x), x) - a*F(x)
    ex1 = dsolve(ex)
    ex2 = ex1.rhs()(Sym("C1") => 1, a => 2)
    @test ex2 == exp(2x)

    t, = @vars t
    X, Y = SymFunction("X, Y")
    eq = [Eq(diff(X(t),t), 12*t*X(t) + 8*Y(t)), Eq(diff(Y(t),t), 21*X(t) + 7*t*Y(t))]
    sympy.dsolve(eq)  # array is not SymbolicObject


    ## version 0.4+ allow use of u'(x) in lieu of diff(u(x), x) and `ivpsolve`
    u = SymFunction("u")
    a, x, y, y0, y1 = symbols("a, x, y, y0, y1")

    @test dsolve(u'(x) - a*u(x), x, (u, 0, 1)) == Eq(u(x), exp(a*x))
    @test dsolve(u'(x) - a*u(x), x, (u, 0, y1)) == Eq(u(x), y1*exp(a*x))
    dsolve(u'(x) - a*u(x), x, (u, y0, y1)) # == Eq(u(x), y1 * exp(a*(x - y0)))
    dsolve(x*u'(x) + x*u(x) + 1, x, (u, 1, 1))
    dsolve((u'(x))^2 - a*u(x), x, (u, 0, 1))
    dsolve(u''(x) - a * u(x), x, (u, 0, 1), (u', 0, 0))

    F, G, K = SymFunction("F, G, K")
    eqn = F(x)*u'(y)*y + G(x)*u(y) + K(x)
    dsolve(eqn, y, (u, 1, 0))

    ## dsolve eqn has two answers, but we want to eliminate one
    # based on initial condition
    dsolve(u'(x) - (u(x)-1)*u(x)*(u(x)+1), x, (u, 0, 1//2))
end
