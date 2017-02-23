using SymPy
using Compat
import Compat.view
if VERSION >= v"0.5.0-dev+7720"
    using Base.Test
else
    using BaseTestNext
    const Test = BaseTestNext
end


@testset "Matrix" begin
    ## matrices
    x, y = @syms x y
    A = [x 1; 1 x]
    B = [x 1; 0 2x]
    v = [x, 2]

    ## These fail for older installations of SymPy
    @test simplify(det(A)) == x^2 - 1

    ## we use inverse for A[:inv]()

    # aliased to use inverse
    @test @compat simplify.(inv(A) * A) ==  eye(2)
    @test @compat simplify.(A * inv(A)) == eye(2)
    @test @compat simplify.(A[:inv]() - inv(A)) == zeros(2, 2)
    @test adjoint(B) == [conj(x) 0; 1 conj(2x)]
    @test adjoint(B) == B'
    @test dual(A) == zeros(2, 2)


    r = cholesky(A)
    @test r*r.' == A


    s = LUsolve(A, v)
    @test @compat simplify.(A * s) == v

    # norm
    @test norm(A) == sqrt(2 * abs(x)^2 + 2)
    # test norm for different subtypes of AbstractArray
    @test norm(A) == norm(Symmetric(A))
    @test norm(A) == norm(view(A, :, :))

    # abs
    @test all(abs(A) .>= 0)
    @test abs(A) == abs(view(A, :, :))

    # is_lower, is_square, is_symmetric much slower than julia only counterparts. May deprecate, but for now they are here
    @test is_lower(A) == istril(A)
    @test is_square(A) == true
    @test is_symmetric(A) == issymmetric(A)
    @test Set(eigvals(A)) == Set([x-1, x+1])


    #numerical tests
    M = Sym[1 0 0; 0 1 0; 0 0 x]
    evecs = eigvecs(M)
    @test evecs[1] == [1, 0, 0]


    M = Sym[1 2 3; 3 6 2; 2 0 1]
    q, r = QRdecomposition(M)
    @test q * r == M
    @test abs(det(q)) == 1


    # for v0.4, the vector type is not correctly inferred
    L = Vector{Sym}[Sym[2,3,5], Sym[3,6,2], Sym[8,3,6]]
    out = GramSchmidt(L, true)
    for i = 1:3, j = i:3
        @test dot(out[i], out[j]) == (i == j ? 1 : 0)
    end

    A = Sym[4 3; 6 3]
    L, U, _ = LUdecomposition(A)
    @test L == Sym[1 0; 3//2 1]

    A = 2eye(Sym, 2)
    B = Sym[1 2; 3 4]
    @test diagonal_solve(A, B) == B/2


    M = Sym[1 2 0; 0 3 0; 2 -4 2]
    P, D = diagonalize(M)
    @test D == [1 0 0; 0 2 0; 0 0 3]
    @test P == [-1 0 -1; 0 0 -1; 2 1  2]
    @test D == inv(P) * M * P

    # test SymPy's expm against Julia's expm
    @test @compat Float64.(expm(M)) ≈ expm(Float64.(M))

    M = [x y; 1 0]
    @test integrate(M, x) == [x^2/2 x*y; x 0]
    @test integrate(M, (x, 0, 2)) == [2 2y; 2 0]


    M = Sym[1 3 0; -2 -6 0; 3 9 6]
    @test nullspace(M)[1] == [-3, 1, 0]


    M = Sym[1 2 0; 0 3 0; 2 -4 2]
    @test cofactor(M, 1, 2) / det(M) == inv(M)[2, 1]
    @test adjugate(M) / det(M) == inv(M)

    M = Sym[ 6  5 -2 -3;
            -3 -1  3  3;
             2  1 -2 -3;
            -1  1  5  5]
    P, J = jordan_form(M)
    @test J == [2 1 0 0;
                0 2 0 0;
                0 0 2 1;
                0 0 0 2]
    @test J == inv(P) * M * P

    ρ, ϕ = symbols("rho, phi")
    X = [ρ*cos(ϕ), ρ*sin(ϕ), ρ^2]
    Y = [ρ, ϕ]
    @test jacobian(X, Y) == [cos(ϕ) -ρ*sin(ϕ);
                             sin(ϕ)  ρ*cos(ϕ);
                             2ρ       0]
    X = [ρ*cos(ϕ), ρ*sin(ϕ)]
    @test jacobian(X, Y) == [cos(ϕ) -ρ*sin(ϕ);
                             sin(ϕ)  ρ*cos(ϕ)]
    @test jacobian(X, Y) == jacobian(view(X, :), view(Y, :))

end
