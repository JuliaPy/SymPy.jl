using SymPy
using Test

@testset "Latexify" begin
  @vars α
  @test_nowarn SymPy.Latexify.latexify(- 3/(8*α) + 1/(8*α^2))
  @test  occursin("\\cdot", SymPy.Latexify.latexify(- 3/(8*α) + 1/(8*α^2), cdot=true))
  @test !occursin("\\cdot", SymPy.Latexify.latexify(- 3/(8*α) + 1/(8*α^2), cdot=false))
end
