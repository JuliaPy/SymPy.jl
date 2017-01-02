#using SymPy.SpecialFuncs
using SymPy
using Base.Test

a, b, n, x = symbols("a, b, n, x")
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

n, m = symbols("n, m")
θ, ϕ = symbols("θ, ϕ")
@test Ynm(n, m, θ, ϕ) == Ynm(n, m, θ, ϕ)
@test Ynm(n, -m, θ, ϕ) == (-1)^m*exp(-2im*m*ϕ)*Ynm(n, m, θ, ϕ)
@test Ynm(n, m, -θ, ϕ) == Ynm(n, m, θ, ϕ)
@test Ynm(n, m, θ, -ϕ) == exp(-2im*m*ϕ)*Ynm(n, m, θ, ϕ)
@test expand(simplify(Ynm(0, 0, θ, ϕ)), func=true) == 1/(2*sqrt(PI))

@test diff(hankel1(n, x), x) == hankel1(n - 1, x)/2 - hankel1(n + 1, x)/2
@test diff(hankel2(n, x), x) == hankel2(n - 1, x)/2 - hankel2(n + 1, x)/2


ν = symbols("ν", integer=true)
@test expand_func(jn(0, x)) == sin(x)/x
@test expand_func(jn(1, x)) == sin(x)/x^2 - cos(x)/x

@test rewrite(jn(ν, x), "besselj") == sqrt(Sym(2)π/x)*besselj(ν + Sym(1)/2, x)/2
#>>> jn(nu, z).rewrite(bessely)
#(-1)**nu*sqrt(2)*sqrt(pi)*sqrt(1/z)*bessely(-nu - 1/2, z)/2

@test N(jn(2, 5.2+0.3im), 20) ≈ 0.099419756723640344491 - 0.054525080242173562897im

@test expand_func(yn(0, x)) == -cos(x)/x
@test expand_func(yn(1, x)) == -cos(x)/x^2-sin(x)/x
#>>> yn(nu, z).rewrite(besselj)
#(-1)**(nu + 1)*sqrt(2)*sqrt(pi)*sqrt(1/z)*besselj(-nu - 1/2, z)/2
#>>> yn(nu, z).rewrite(bessely)
#sqrt(2)*sqrt(pi)*sqrt(1/z)*bessely(nu + 1/2, z)/2
#>>> yn(2, 5.2+0.3j).evalf(20)
#0.18525034196069722536 + 0.014895573969924817587*I

