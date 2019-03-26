using SymPy
using Test
using LinearAlgebra

@testset "Matrix" begin
    ## matrices
    x, y = @vars x y
    A = [x 1; 1 x]
    B = [x 1; 0 2x]
    v = [x, 2]

    ## These fail for older installations of SymPy
    @test simplify(det(A)) == x^2 - 1

    ## we use inverse for A[:inv]()

    # aliased to use inverse
    @test simplify.(inv(A) * A) ==  [1 0; 0 1]
    @test simplify.(A * inv(A)) == [1 0; 0 1]
    ##XXX    @test simplify.(A[:inv]() - inv(A)) == zeros(2, 2)

    @test SymPy.adjoint(B) == [adjoint(x) 0; 1 adjoint(2x)]
    @test SymPy.adjoint(B) == B'
    @test A.dual() == sympy.zeros(2, 2)


    A1 = Sym[25 15 -5; 15 18 0; -5 0 11]
    M1 = convert(SymMatrix, A1)
    r = A1.cholesky()
    @test r*r.transpose() == M1


    #    s = LUsolve(A, v)
    M, V = convert.(SymMatrix, (A, v))
    s = M.LUsolve(V)
    @test simplify(M * s) == V

    # norm
    @test norm(A) == sqrt(2 * abs(x)^2 + 2)
    # test norm for different subtypes of AbstractArray
    ## XXX @test norm(A) == norm(Symmetric(A)) LinearAlgebra.Symmetric no long works
    @test norm(A) == norm(view(A, :, :))

    # abs
    @test  all(convert.(Bool, abs.(A) .≧ 0))
    @test  abs.(A) == abs.(view(A, :, :))

    # is_lower, is_square, is_symmetric much slower than julia only counterparts. May deprecate, but for now they are here
    @test A.is_lower == istril(A)
    @test A.is_square == true
    @test A.is_symmetric() != issymmetric(A)

    @vars x real=true
    A = [x 1; 1 x]
    @test A.is_symmetric() == issymmetric(A)

    @test Set(eigvals(A)) == Set([x-1, x+1])


    #numerical tests
    M = Sym[1 0 0; 0 1 0; 0 0 x]
    evecs = eigvecs(M)
    @test evecs[:,1] == [1, 0, 0]


    A = Sym[1 2 3; 3 6 2; 2 0 1]
    M = convert(SymMatrix, A)
    q, r = A.QRdecomposition()
    @test q * r == M
    @test abs(det(q)) == 1


    # for v0.4, the vector type is not correctly inferred
    #L = Vector{Sym}[Sym[2,3,5], Sym[3,6,2], Sym[8,3,6]]
    #out = GramSchmidt(L, true)
    L = collect(sympy.Matrix.(([[2,3,5]], [[3,6,2]], [[8,3,6]])))
    out = sympy.GramSchmidt(L, true)  # qualify, as L not SymbolicObject type
    for i = 1:3, j = i:3
        @test out[i].dot(out[j]) == (i == j ? 1 : 0)
    end

    A = convert(SymMatrix, Sym[4 3; 6 3])
    L, U, _ = A.LUdecomposition()
    @test L == convert(SymMatrix, Sym[1 0; 3//2 1])

    A = Sym[1 0; 0 1] * 2
    B = Sym[1 2; 3 4]
    A1, B1 = convert.(SymMatrix, (A,B))
    @test A.diagonal_solve(B) == B1/2

    M = Sym[1 2 0; 0 3 0; 2 -4 2]
    P1, D1 = M.diagonalize()
    P, D = convert.(Matrix{Sym}, (P1, D1))
    @test D == [1 0 0; 0 2 0; 0 0 3]
    @test P == [-1 0 -1; 0 0 -1; 2 1  2]
    @test D == inv(P) * M * P

    # test SymPy's expm against Julia's expm
    A = [1 2 0; 0 3 0; 2 -4 2]
    M = Sym.(A)
    M1 = convert(SymMatrix,M)
    ## no exp(M)!
    U = convert(Matrix{Sym}, M.exp()) - exp(A)
    @test maximum(abs.(N.(U))) <= 1e-12

    M = [x y; 1 0]
    @test integrate.(M, x) == [x^2/2 x*y; x 0]
    @test integrate.(M, Ref((x, 0, 2))) == [2 2y; 2 0]


    M = Sym[1 3 0; -2 -6 0; 3 9 6]
    @test M.nullspace()[1] == convert(SymMatrix, [-3, 1, 0])


    M = Sym[1 2 0; 0 3 0; 2 -4 2]
    @test M.cofactor(1, 2) / M.det() == M.inv()[2, 1]
    @test M.adjugate() / M.det() == M.inv()

    M = Sym[ 6  5 -2 -3;
            -3 -1  3  3;
             2  1 -2 -3;
            -1  1  5  5]

    P1, J1 = M.jordan_form()
    P, J = convert.(Matrix{Sym}, (P1, J1))
    @test J == [2 1 0 0;
                0 2 0 0;
                0 0 2 1;
                0 0 0 2]
    @test J == inv(P) * M * P

    ρ, ϕ = symbols("rho, phi")
    X = [ρ*cos(ϕ), ρ*sin(ϕ), ρ^2]
    Y = [ρ, ϕ]
    X1, Y1 = convert.(SymMatrix, (X,Y))
    @test X.jacobian(Y) == convert(SymMatrix,
                                    [cos(ϕ) -ρ*sin(ϕ);
                                     sin(ϕ)  ρ*cos(ϕ);
                                     2ρ       0])
    X = [ρ*cos(ϕ), ρ*sin(ϕ)]
    @test convert(Matrix{Sym}, X.jacobian(Y)) == [cos(ϕ) -ρ*sin(ϕ);
                                                  sin(ϕ)  ρ*cos(ϕ)]
    @test X.jacobian(Y) == view(X, :, :).jacobian(view(Y, :, :))

end
