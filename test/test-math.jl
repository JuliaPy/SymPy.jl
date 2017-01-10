using SymPy
using SymPy: symbols, sqrt, simplify, PI
using Base.Test

ρ = symbols("rho", positive=true)
ϕ = symbols("phi", real=true)

@test simplify(hypot(ρ*cos(ϕ), ρ * sin(ϕ))) == ρ

@test atan2(Sym(1), Sym(1)) == PI/4
@test atan2(Sym(1), -Sym(1)) == 3PI/4
