using SymPy
using SymPy: symbols, sqrt, simplify, PI
using Base.Test

ρ = symbols("rho", positive=true)
ϕ = symbols("phi", real=true)

@test simplify(hypot(ρ*cos(ϕ), ρ * sin(ϕ))) == ρ
@test hypot(ρ*cos(ϕ), 3) == sqrt(ρ^2*cos(ϕ)^2 + 9)

@test atan2(Sym(1), 1) == PI/4
@test atan2(Sym(1), -1) == 3PI/4
