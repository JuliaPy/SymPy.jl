using SpecialFunctions
using SymPy
using SymPy.SpecialFuncs
using SymPy: Sym, sqrt, conjugate, symbols, PI, simplify,
             expand_func, rewrite, N, gamma
#using Test


@testset "SpecialFuns" begin
    a, b, x, y = symbols("a, b, x, y")
    n, m, θ, ϕ = symbols("n, m, theta, phi")
    ν = symbols("nu", integer=true)


    @test fresnels(Sym(0)) == 0
    @test fresnels(Sym(oo)) == Sym(1)/2
    @test diff(fresnels(x), x) == sin(PI*x^2/2)
    #@test evalf(fresnels(Sym(2)), 30) == Sym(parse(BigFloat, "0.343415678363698242195300815958"))


    @test  diff(fresnelc(x), x) == cos(PI*x^2/2)


    @test Ei(Sym(-1)) == Ei(exp(1im*PI))
    @test diff(Ei(x), x) == exp(x)/x
    @test diff(Si(x), x) == sin(x)/x
    @test diff(Ci(x), x) == cos(x)/x


    @test airyai(Sym(0)) == 3^(Sym(1)/3)/(3*gamma(Sym(2)/3))
    @test airybi(Sym(0)) == 3^(Sym(5)/6)/(3*gamma(Sym(2)/3))


    @test jacobi(0, a, b, x) == 1
    @test jacobi(1, a, b, x) == a/2 - b/2 + x*(a/2 + b/2 + 1)
    @test jacobi(n, 0, 0, x) == legendre(n, x)
    @test jacobi(n, a, b, -x) == (-1)^n*jacobi(n, b, a, x)
    @test conjugate(jacobi(n, a, b, x)) == jacobi(n, conjugate(a), conjugate(b), conjugate(x))


    @test gegenbauer(0, a, x) == 1
    @test gegenbauer(1, a, x) == 2a*x
    @test gegenbauer(2, a, x) == -a + x^2*(2a^2 + 2a)
    @test conjugate(gegenbauer(n, a, x)) == gegenbauer(n, conjugate(a), conjugate(x))
    @test diff(gegenbauer(n, a, x), x) == 2a*gegenbauer(n-1, a+1, x)


    @test chebyshevt(0, x) == 1
    @test chebyshevt(1, x) == x
    @test chebyshevt(2, x) == 2x^2-1


    @test chebyshevu(0, x) == 1
    @test chebyshevu(1, x) == 2x
    @test chebyshevu(2, x) == 4x^2 - 1
    @test chebyshevu(n, 0) == cos(π*n/2)
    @test chebyshevu(n, 1) == n + 1


    @test legendre(0, x) == 1
    @test legendre(1, x) == x
    @test legendre(2, x) == 3x^2/2 - 1/2
    @test legendre(n, x) == legendre(n, x)
    @test diff(legendre(n,x), x) == n*(x*legendre(n, x) - legendre(n - 1, x))/(x^2 - 1)


    @test assoc_legendre(0,0, x) == 1
    @test assoc_legendre(1,0, x) == x
    @test assoc_legendre(1,1, x) == -sqrt(-x^2 + 1)
    #@test assoc_legendre(n,m,x) == assoc_legendre(n, m, x)


    @test hermite(0, x) == 1
    @test hermite(1, x) == 2x
    @test hermite(2, x) == 4x^2 - 2
    @test hermite(n, x) == hermite(n, x)
    @test diff(hermite(n,x), x) == 2n*hermite(n - 1, x)
    @test hermite(n, -x) == (-1)^n*hermite(n, x)


    @test laguerre(0, x) == 1
    @test laguerre(1, x) == -x + 1
    @test laguerre(2, x) == x^2/2 - 2x + 1
    @test laguerre(3, x) == -x^3/6 + 3x^2/2 - 3x + 1
    @test diff(laguerre(n, x), x) == -assoc_laguerre(n - 1, 1, x)


    @test assoc_laguerre(0, a, x) == 1
    @test assoc_laguerre(1, a, x) == a - x + 1
    @test assoc_laguerre(2, a, x) == a^2/2 + 3a/2 + x^2/2 + x*(-a - 2) + 1
    @test assoc_laguerre(n, 0, x) == laguerre(n, x)
    @test diff(assoc_laguerre(n, a, x), x) == -assoc_laguerre(n - 1, a + 1, x)


    @test Ynm(n, m, θ, ϕ) == Ynm(n, m, θ, ϕ)
    @test Ynm(n, -m, θ, ϕ) == (-1)^m*exp(-2im*m*ϕ)*Ynm(n, m, θ, ϕ)
    @test Ynm(n, m, -θ, ϕ) == Ynm(n, m, θ, ϕ)
    @test Ynm(n, m, θ, -ϕ) == exp(-2im*m*ϕ)*Ynm(n, m, θ, ϕ)
    @test expand(simplify(Ynm(0, 0, θ, ϕ)), func=true) == 1/(2*sqrt(PI))

    @test Ynm_c(n, m, θ, ϕ) == conjugate(Ynm(n, m, θ, ϕ))

    @test diff(hankel1(n, x), x) == hankel1(n - 1, x)/2 - hankel1(n + 1, x)/2
    @test diff(hankel2(n, x), x) == hankel2(n - 1, x)/2 - hankel2(n + 1, x)/2


    @test expand_func(jn(0, x)) == sin(x)/x
    @test expand_func(jn(1, x)) == sin(x)/x^2 - cos(x)/x
    @test rewrite(jn(ν, x), "besselj") == sqrt(2PI/x)*besselj(ν + Sym(1)/2, x)/2
    VERSION < v"0.6.0-dev" && @test rewrite(jn(ν, x), "bessely") == (-1)^ν*sqrt(2PI/x)*bessely(-ν - Sym(1)/2, x)/2
    u = N(jn(2, 5.2+0.3im), 20)
    @test norm(real(u) - 0.099419756723640344491) <= 1e-15 && norm(imag(u) + 0.054525080242173562897) <= 1e-15


    @test expand_func(yn(0, x)) == -cos(x)/x
    @test expand_func(yn(1, x)) == -cos(x)/x^2-sin(x)/x
    VERSION < v"0.6.0-dev" && @test rewrite(yn(ν, x), "besselj") == (-1)^(ν + 1)*sqrt(2PI/x)*besselj(-ν - Sym(1)/2, x)/2
    @test rewrite(yn(ν, x), "bessely") == sqrt(2PI/x)*bessely(ν + Sym(1)/2, x)/2
    @test N(yn(2, 5.2+0.3im)) ≈ 0.18525034196069722536 + 0.014895573969924817587im


    # gamma, beta and related functions
    @test gamma(Sym(1)) == 1
    @test gamma(Sym(3)/2) == sqrt(PI)/2


    @test diff(polygamma(Sym(0), x), x) == polygamma(Sym(1), x)
    @test diff(polygamma(Sym(0), x), x, 2) == polygamma(Sym(2), x)


    @test diff(beta(x, y), x) == (polygamma(Sym(0), x) - polygamma(Sym(0), x + y)) * beta(x, y)
    @test diff(beta(x, y), y) == (polygamma(Sym(0), y) - polygamma(Sym(0), x + y)) * beta(x, y)


    # test numerical consistency with Julia functions
    @test N(gamma(Sym(4.1))) ≈ gamma(4.1)
    @test N(polygamma(Sym(2), Sym(3.2))) ≈ polygamma(2, 3.2)
    VERSION >= v"0.5.0" && @test N(beta(Sym(1)+1im, Sym(1)+1im)) ≈ beta(1.0+1im, 1.0+1im)


    # Elliptic-type functions

    @test elliptic_k(Sym(0)) == PI/2
    @test N(elliptic_k(Sym(1.0 + im))) ≈ 1.50923695405127 + 0.625146415202697im

    @test N(elliptic_f(Sym(3.0 + im/2), Sym(1.0 + im))) ≈ 2.909449841483 + 1.74720545502474im

    @test elliptic_e(Sym(0)) == PI/2
    @test N(elliptic_e(Sym(2.0 - im))) ≈ 0.991052601328069 + 0.81879421395609im

    @test elliptic_pi(Sym(0), Sym(0)) == PI/2
    @test N(elliptic_pi(Sym(1.0 - im/3), Sym(2.0 + im))) ≈ 3.29136443417283 + 0.32555634906645im


    # Bessel-type functions
    @test diff(besselj(n, x), x) == (besselj(n - 1, x) - besselj(n + 1, x))/2
    @test rewrite(besselj(n, x), "jn") == sqrt(2x/PI)*jn(n - 1/2, x)


    @test diff(bessely(n, x), x) == (bessely(n - 1, x) - bessely(n + 1, x))/2
    @test rewrite(bessely(n, x), "yn") == sqrt(2x/PI)*yn(n - 1/2, x)


    @test diff(besseli(n, x), x) == (besseli(n - 1, x) + besseli(n + 1, x))/2


    @test diff(besselk(n, x), x) == -(besselk(n - 1, x) + besselk(n + 1, x))/2


    # test numerical consistency with Julia functions
    if VERSION < v"0.6.0-dev"
      @test N(besselj(3.2, Sym(1.5))) ≈ besselj(3.2, 1.5)
      @test N(bessely(3.2, Sym(1.5))) ≈ bessely(3.2, 1.5)
      @test N(besseli(3.2, Sym(1.5))) ≈ besseli(3.2, 1.5)
      @test N(besselk(3.2, Sym(1.5))) ≈ besselk(3.2, 1.5)
    end

    @test expand_func(x*hyper([1, 1], [2], -x)) == log(x + 1)
end
