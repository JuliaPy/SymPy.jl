using SymPy
using Base.Test


@testset "Matrix" begin
    ## matrices
    (x,) = @syms x
    A = [x 1; 1 x]
    b = [x, 2]


    ## These fail for older installations of SymPy
    @test simplify(det(A)) == x^2 - 1

    ## we use inverse for A[:inv]()
    inv(A) # aliased to use inverse
    @test simplify((A[:inv]() - inv(A))[1,1]) == 0
    
    adjoint(A)
    dual(A)
    cholesky(A)
    ## other functions, could wrap
    b = subs(A, x, 2)
    QRdecomposition(b)

    # is_lower, is_square, is_symmetric much slower than julia only counterparts. May deprecate, but for now they are here
    @test is_lower(A) == istril(A)
    @test is_square(A) == true
    @compat @test is_symmetric(A) == issymmetric(A)
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
