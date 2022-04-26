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


    ## Removed
    ## version 0.4+ allow use of u'(x) in lieu of diff(u(x), x) and `ivpsolve`
    ## This will be deprecated in favor of Differential(x)
    u = SymFunction("u")
    a, x, y, y0, y1 = symbols("a, x, y, y0, y1")

    @test dsolve(u'(x) - a*u(x), u(x), ics=(u, 0, 1)) == Eq(u(x), exp(a*x))
    @test dsolve(u'(x) - a*u(x), u(x), ics=(u, 0, y1)) == Eq(u(x), y1*exp(a*x))
    dsolve(u'(x) - a*u(x), u(x), ics=(u, y0, y1)) # == Eq(u(x), y1 * exp(a*(x - y0)))
    dsolve(x*u'(x) + x*u(x) + 1, u(x), ics=(u, 1, 1))
    dsolve((u'(x))^2 - a*u(x), u(x), ics=(u, 0, 1))
    dsolve(u''(x) - a * u(x), u(x), ics=((u, 0, 1), (u', 0, 0)))

    F, G, K = SymFunction("F, G, K")
    eqn = F(x)*u'(y)*y + G(x)*u(y) + K(x)
    dsolve(eqn, u(y), ics=(u, 1, 0))

    ## dsolve eqn has two answers, but we want to eliminate one
    # based on initial condition
    dsolve(u'(x) - (u(x)-1)*u(x)*(u(x)+1), u(x), ics=(u, 0, 1//2))

    ## ---

    ## use Differential, not u' or u''
    ## use Dict to specify ics from SymPy, not internal one
    @syms a x y0 y1 u()
    ∂ = Differential(x)

    @test dsolve(∂(u)(x) - a*u(x), u(x), ics=Dict(u(0) => 1)) == Eq(u(x), exp(a*x))
    @test dsolve(∂(u)(x) - a*u(x), u(x), ics=Dict(u(0) => y1)) == Eq(u(x), y1*exp(a*x))
    dsolve(∂(u)(x) - a*u(x), u(x), ics=Dict(u(y0)=>y1)) # == Eq(u(x), y1 * exp(a*(x - y0)))
    dsolve(x*∂(u)(x) + x*u(x) + 1, u(x), ics=Dict(u(1) => 1))
    𝒂 = 2
    dsolve((∂(u)(x))^2 - 𝒂 * u(x), u(x), ics=Dict(u(0) => 0, ∂(u)(0) => 0))
    dsolve(∂(∂(u))(x) - 𝒂 * u(x), u(x), ics=Dict(u(0)=> 1, ∂(u)(0) => 0))

    F, G, K = SymFunction("F, G, K")
    eqn = F(x)*∂(u)(y)*y + G(x)*u(y) + K(x)
    dsolve(eqn, u(y), ics=Dict(u(1) => 0))

    ## dsolve eqn has two answers, but we want to eliminate one
    # based on initial condition
    dsolve(∂(u)(x) - (u(x)-1)*u(x)*(u(x)+1), u(x), ics=Dict(u(0)=> Sym(1//2)))

    ## ----
    ## rhs works
    u = SymFunction("u")
    @syms x y a::positive
    eqn = ∂(u)(x) - a * u(x) * (1 - u(x))
    out = dsolve(eqn)
    eq = rhs(out)    # just the right hand side
    C1 = first(setdiff(free_symbols(eq), (x,a)))
    c1 = solve(eq(x=>0) - 1//2, C1)
    @test c1[1] == Sym(1)


    ## dsolve and system of equations issue #291
    @syms t x() y()
    ∂ = Differential(t)
    eq1 = ∂(x(t)) ~ x(t)*y(t)*sin(t)
    eq2 = ∂(y(t)) ~ y(t)^2*sin(t)
#    eq1 = Eq(diff(x(t),t),x(t)*y(t)*sin(t))
#    eq2 = Eq(diff(y(t),t),y(t)^2*sin(t))
    out = dsolve([eq1, eq2]) # vector
    out = dsolve((eq1, eq2)) # tuple
    Dict(lhs.(collect(out)) .=> rhs.(collect(out))) # turn python set into a dictionary

end
