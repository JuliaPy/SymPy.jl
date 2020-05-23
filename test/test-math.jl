using SymPy
using Test
using SpecialFunctions

@testset "Math" begin
    ρ = symbols("rho", positive=true)
    ϕ = symbols("phi", real=true)
    x = symbols("x")

    @test simplify(hypot(ρ*cos(ϕ), ρ * sin(ϕ))) == ρ
    @test simplify(hypot(ρ*cos(ϕ), 3)) == sqrt(ρ^2*cos(ϕ)^2 + 9)

    @test atan(Sym(1), 1) == PI/4
    @test atan(Sym(1), -1) == 3PI/4

    @test factorial(Sym(0)) == 1
    @test factorial(Sym(7)) == 5040

    @test sympy.factorial2(Sym(5)) == 15
    @test sympy.factorial2(Sym(-5)) == Sym(1)/3

    @test erf(Sym(0)) == 0
    @test erf(Sym(oo)) == 1
    @test diff(erf(x), x) == 2*exp(-x^2)/sqrt(PI)


    @test sinc(Sym(0)) == 1
    # test consistency with Julia's sinc
    @test sinc(Sym(1)) == 0
    @test N(sinc(Sym(0.2))) ≈ sinc(0.2)

    @test flipsign(Sym(3), 2.0) == 3
    @test flipsign(Sym(3), 0.0) == 3
    @test flipsign(Sym(3), -0.0) == -3
    @test flipsign(Sym(3), -2.0) == -3

    @test eps(Sym) == 0
    #@test rewrite(sinc(x), "jn") == jn(0, PI * x)
end

@testset  "nan"  begin
    #  issue  346
    a=sympy.nan
    a′ = NaN

    @test (a < 0) == (a′ < 0)
    @test (a > 0) == (a′ > 0)
    @test (a == 0) == (a′ == 0)

end
