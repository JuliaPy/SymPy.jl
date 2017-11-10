using SymPy.Physics
using SymPy: Sym, sqrt, symbols, doit, PI
using SymPy.SpecialFuncs: Ynm
using PyCall: PyError
#using Test


@testset "Physics" begin
    @test clebsch_gordan(Sym(3)/2, Sym(1)/2, 2, Sym(3)/2, Sym(1)/2, 2) == 1
    @test clebsch_gordan(Sym(3)/2, Sym(1)/2, 1, Sym(3)/2, -Sym(1)/2, 1) == sqrt(Sym(3))/2
    @test clebsch_gordan(Sym(3)/2, Sym(1)/2, 1, -Sym(1)/2, Sym(1)/2, 0) == -sqrt(Sym(2))/2

    θ, ϕ = symbols("theta, phi")
    @test doit(dot_rot_grad_Ynm(3, 2, 2, 0, θ, ϕ)) == 3*sqrt(Sym(55))*Ynm(5, 2, θ, ϕ)/(11*sqrt(PI))

    @test gaunt(1,0,1,1,0,-1) == -1/(2*sqrt(PI))
    #@test N(gaunt(1000,1000,1200,9,3,-12)) ≈ 0.00689500421922113448 # takes forever to compute

    @test_throws PyError gaunt(1.2,0,1.2,0,0,0)
    @test_throws PyError gaunt(1,0,1,1.1,0,-1.1)

    @test racah(3,3,3,3,3,3) == -1//14


    @test wigner_3j(2, 6, 4, 0, 0, 0) == sqrt(Sym(715))/143
    @test wigner_3j(2, 6, 4, 0, 0, 1) == 0

    @test wigner_6j(3,3,3,3,3,3) == -1//14
    @test wigner_6j(5,5,5,5,5,5) == 1//52


    # Optics
    @test RayTransferMatrix(1,2,3,4) == Sym[1 2; 3 4]
    @test FlatMirror() == Sym[1 0; 0 1]
end
