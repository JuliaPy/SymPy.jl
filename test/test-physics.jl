using SymPy
using PyCall: PyError
using Test


import PyCall
PyCall.pyimport_conda("sympy.physics.wigner",       "sympy")
PyCall.pyimport_conda("sympy.physics.optics",       "sympy")
PyCall.pyimport_conda("sympy.physics.quantum.spin", "sympy")

import_from(sympy.physics.wigner)
import_from(sympy.physics.optics)
import_from(sympy.physics.quantum.spin)

@testset "Physics" begin
    @test clebsch_gordan(Sym(3)/2, Sym(1)/2, 2, Sym(3)/2, Sym(1)/2, 2) == 1
    @test clebsch_gordan(Sym(3)/2, Sym(1)/2, 1, Sym(3)/2, -Sym(1)/2, 1) == sqrt(Sym(3))/2
    @test clebsch_gordan(Sym(3)/2, Sym(1)/2, 1, -Sym(1)/2, Sym(1)/2, 0) == -sqrt(Sym(2))/2

    θ, ϕ = symbols("theta, phi")
    @test dot_rot_grad_Ynm(Sym(3), 2, 2, 0, θ, ϕ).doit() == 3*sqrt(Sym(55))*Ynm(Sym(5), 2, θ, ϕ)/(11*sqrt(PI))

    @test gaunt(Sym(1),0,1,1,0,-1) == -1/(2*sqrt(PI))
    @test N(gaunt(Sym(1000),1000,1200,9,3,-12)) ≈ 0.00689500421922113448 # takes forever to compute

    @test_throws PyError gaunt(Sym(1),2,0,1.2,0,0,0)
    @test_throws PyError gaunt(Sym(1),0,1,1.1,0,-1.1)

    @test racah(Sym(3),3,3,3,3,3) == -1//14


    @test wigner_3j(Sym(2), 6, 4, 0, 0, 0) == sqrt(Sym(715))/143
    @test wigner_3j(Sym(2), 6, 4, 0, 0, 1) == 0

    @test wigner_6j(Sym(3),3,3,3,3,3) == -1//14
    @test wigner_6j(Sym(5),5,5,5,5,5) == 1//52


    # Optics
    # sympy.physics.optics.RayTransferMatrix not a functino
    @test sympy.physics.optics.RayTransferMatrix(1,2,3,4) == convert(SymMatrix,Sym[1 2; 3 4])
    @test sympy.physics.optics.FlatMirror() == convert(SymMatrix,Sym[1 0; 0 1])

    # Spin
    import_from(sympy.physics.quantum.spin, (:WignerD,), typ=:Any)
    @test WignerD(Sym(3), Sym(2), Sym(1), PI, PI/2, -PI).doit() == -sqrt(Sym(10))/8
    @test WignerD(Sym(1), Sym(1), Sym(0), 0, θ, 0).doit() == -sin(θ)/sqrt(Sym(2))
end
