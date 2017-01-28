using SymPy
if VERSION >= v"0.5.0-dev+7720"
    using Base.Test
else
    using BaseTestNext
    const Test = BaseTestNext
end


@testset "Matrix" begin
    ## matrices
    x, = @syms x
    A = [x 1; 1 x]
    b = [x, 2]

    #@test a*a == A*A

    ## These fail for older installations of SymPy
    @test simplify(det(A)) == x^2 - 1

    ## we use inverse for A[:inv]()

    # aliased to use inverse
    @test @compat simplify.(inv(A) * A) ==  [1 0; 0 1]
    @test @compat simplify.(A * inverse(A)) == [1 0 ; 0 1]
    @test @compat simplify(A[:inv]() - inv(A)) == 0
    a[:inv]() |> u -> convert(Array{Sym}, u)
    a[:inv]("LU")                   # pass argument to function
    @test adjoint(A) == [conj(x) 1; 1 conj(x)]
    @test adjoint(A) == A'
    dual(A)

    ## other functions, could wrap
    let
        r = cholesky(A)
        @test r*r.' == A
        b = subs(A, x, 2)
        q, r = QRdecomposition(b)
        @test q * r == b
        @test det(q) == 1
    end

    # is_lower, is_square, is_symmetric much slower than julia only counterparts. May deprecate, but for now they are here
    @test is_lower(A) == istril(A)
    @test is_square(A) == true
    @test is_symmetric(A) == issymmetric(A)
    @test Set(eigvals(A)) == Set([x-1, x+1])


    A = [1 0 0; 0 1 0; 0 0 x]
    evecs = eigvecs(A)
    @test evecs[1] == [1, 0, 0]


    ##
    M = Sym[1 2 3; 3 6 2; 2 0 1]
    q,r = QRdecomposition(M)
    @test (q * r - M)[1,1] == 0
    if VERSION >= v"0.5"
        L = [Sym[2,3,5], Sym[3,6,2], Sym[8,3,6]]
        out = GramSchmidt(L)
    end
    A = Sym[4 3; 6 3]
    L, U, _ = LUdecomposition(A)
    @test L == Sym[1 0; 3//2 1]


end
