if lowercase(get(ENV, "CI", "false")) == "true"
    include("install_dependencies.jl")
end


using SymPy

path = joinpath(pathof(SymPy.SymPyCore), "../../test")
include(joinpath(path, "runtests-sympycore.jl"))

using Test
@testset "hash (issue with PyCall GIL release)" begin
    @syms c() a b
    d = Dict(c(0) => a)
    @test d[c(0)] == a
    @test hash(a + b) == hash(b + a)
    @test length(Set([a, a, c(0), c(0)])) == 2
end
