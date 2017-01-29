using SymPy
if VERSION >= v"0.5.0-dev+7720"
    using Base.Test
else
    using BaseTestNext
    const Test = BaseTestNext
end


@testset "Matrix" begin
    ## matrices
    (x,) = @syms x
    A = [x 1; 1 x]
    a = convert(SymMatrix, A)
    b = [x, 2]


    ## These fail for older installations of SymPy
    @test simplify(det(A)) == x^2 - 1
    @test simplify(det(a)) == x^2 - 1

    ## we use inverse for A[:inv]()
    inv(A) # aliased to use inverse
    inverse(A)
    a[:inv]() |> u -> convert(Array{Sym}, u)
    a[:inv]("LU")                   # pass argument to function
    adjoint(A)
    dual(A)
    cholesky(A)
    ## other functions, could wrap
    b = subs(a, x, 2)
    QRdecomposition(b)

    for m in (a, A)
        @test is_lower(m) == istril(A)
        @test is_square(m) == true
        @test is_symmetric(m) == issymmetric(A)
    end

    @test eigvals(A) == [x-1, x+1]


    a = [1 0 0; 0 1 0; 0 0 x]
    evecs = eigvecs(a)
    @test float(rref(evecs)) == eye(3)

    eh = convert(SymMatrix, a)
    @test eh[9] == x
    convert(SymMatrix, reshape([x, 1:23...], (2,3,4)))
end
