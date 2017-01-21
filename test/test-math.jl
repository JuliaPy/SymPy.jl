using SymPy
using SymPy.SpecialFuncs: jn
using Base.Test

ρ = symbols("rho", positive=true)
ϕ = symbols("phi", real=true)
x = symbols("x")

@test simplify(hypot(ρ*cos(ϕ), ρ * sin(ϕ))) == ρ
@test hypot(ρ*cos(ϕ), 3) == sqrt(ρ^2*cos(ϕ)^2 + 9)

@test atan2(Sym(1), 1) == PI/4
@test atan2(Sym(1), -1) == 3PI/4

@test factorial(Sym(0)) == 1
@test factorial(Sym(7)) == 5040

@test factorial2(Sym(5)) == 15
@test factorial2(Sym(-5)) == Sym(1)/3

@test erf(Sym(0)) == 0
@test erf(Sym(oo)) == 1
@test diff(erf(x), x) == 2*exp(-x^2)/sqrt(PI)


@test sinc(Sym(0)) == 1
@test diff(sinc(x), x) == (x*cos(x) - sin(x))/x^2
@test rewrite(sinc(x), "jn") == jn(0, x)
