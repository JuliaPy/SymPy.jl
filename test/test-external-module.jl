using SymPy
import PyCall
using Test

## Test of importing an external module using `pyimport_conda` and `import_from`.
## from issue #262

stats = PyCall.pyimport_conda("sympy.stats", "sympy")

# this brings in all functions into the session using introspection, specialized on a
# symbolic first argument
import_from(stats)


@testset "Test stats module" begin

    @vars mu z x
    @vars sigma positive=true
    X = stats.Normal("x", mu, sigma)

    # some operations:
    @test variance(X) == sigma^2
    @test E(X) == mu
    P(Gt(X,3)) ## or P(X ≫ 3)

    Z = stats.Normal("Z", 0, 1)  # needs `stats.` as "Z" is not symbolic,
    # could also do this:
    @vars Z
    Z = Normal(Z, 0, 1)
    @test E(Z) == 0
    @test variance(Z) == 1

    simplify(P(Gt(Z, 1)))

    @test 0.68 <= 1 - 2*N(P(Gt(Z, 1))) <= 0.69


    @vars Y
    @vars a b c
    Y = DiscreteUniform(Y, (a,b,c))
    density(Y).dict
    @test density(Y).dict[a] == 1//3

    # And density
    density(X)  #NormalDistribution(mu, sigma)
    density(X).pdf(z)  # need the `pdf` call here

end
