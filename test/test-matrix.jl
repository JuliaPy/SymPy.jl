using SymPy
if VERSION >= v"0.5.0-dev+7720"
    using Base.Test
else
    using BaseTestNext
    const Test = BaseTestNext
end
#using Base.Test


@testset "Matrix" begin

    ## matrices
    (x,) = @syms x
    A = [x 1; 1 x]
    a = convert(SymMatrix, A) ## for [:meth] calls
    b = [x, 2]


    ## These fail for older installations of SymPy, such as are present in the travis test environment

    det(A)
    det(a)

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

    @assert is_square(a) == true
    @assert is_symmetric(a) == true


    eigvals(A)



    a = [1 0 0; 0 1 0; 0 0 x]
    evecs = eigvecs(a)
    @assert float(rref(evecs)) == eye(3)

    eh = convert(SymMatrix, a)
    eh[9]
    convert(SymMatrix, reshape([x, 1:23...], (2,3,4)))
end
