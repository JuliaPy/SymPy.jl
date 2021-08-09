### These are generated by a) uncommenting import_sympy() in __init__ b) uncomment println parts in import_from
expj(ex::SymbolicObject, args...; kwargs...)=getproperty(mpmath, :expj)(ex, Sym.(args)...; kwargs...); export expj
fac(ex::SymbolicObject, args...; kwargs...)=getproperty(mpmath, :fac)(ex, Sym.(args)...; kwargs...); export fac
nint(ex::SymbolicObject, args...; kwargs...)=getproperty(mpmath, :nint)(ex, Sym.(args)...; kwargs...); export nint
Base.ceil(ex::SymbolicObject, args...; kwargs...)=getproperty(mpmath, :ceil)(ex, Sym.(args)...; kwargs...)
fib(ex::SymbolicObject, args...; kwargs...)=getproperty(mpmath, :fib)(ex, Sym.(args)...; kwargs...); export fib
monitor(ex::SymbolicObject, args...; kwargs...)=getproperty(mpmath, :monitor)(ex, Sym.(args)...; kwargs...); export monitor
Base.cospi(ex::SymbolicObject, args...; kwargs...)=getproperty(mpmath, :cospi)(ex, Sym.(args)...; kwargs...)
bernfrac(ex::SymbolicObject, args...; kwargs...)=getproperty(mpmath, :bernfrac)(ex, Sym.(args)...; kwargs...); export bernfrac
doctests(ex::SymbolicObject, args...; kwargs...)=getproperty(mpmath, :doctests)(ex, Sym.(args)...; kwargs...); export doctests
ei(ex::SymbolicObject, args...; kwargs...)=getproperty(mpmath, :ei)(ex, Sym.(args)...; kwargs...); export ei
Base.sinpi(ex::SymbolicObject, args...; kwargs...)=getproperty(mpmath, :sinpi)(ex, Sym.(args)...; kwargs...)
timing(ex::SymbolicObject, args...; kwargs...)=getproperty(mpmath, :timing)(ex, Sym.(args)...; kwargs...); export timing
rgamma(ex::SymbolicObject, args...; kwargs...)=getproperty(mpmath, :rgamma)(ex, Sym.(args)...; kwargs...); export rgamma
expjpi(ex::SymbolicObject, args...; kwargs...)=getproperty(mpmath, :expjpi)(ex, Sym.(args)...; kwargs...); export expjpi
ellipk(ex::SymbolicObject, args...; kwargs...)=getproperty(mpmath, :ellipk)(ex, Sym.(args)...; kwargs...); export ellipk
e1(ex::SymbolicObject, args...; kwargs...)=getproperty(mpmath, :e1)(ex, Sym.(args)...; kwargs...); export e1
SpecialFunctions.digamma(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :digamma)(ex, Sym.(args)...; kwargs...)
#Base.rem(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :rem)(ex, Sym.(args)...; kwargs...)
Base.asin(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :asin)(ex, Sym.(args)...; kwargs...)
Base.acosh(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :acosh)(ex, Sym.(args)...; kwargs...)
Base.im(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :im)(ex, Sym.(args)...; kwargs...)
Base.collect(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :collect)(ex, Sym.(args)...; kwargs...)
Base.Function(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :Function)(ex, Sym.(args)...; kwargs...)
SpecialFunctions.erfi(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :erfi)(ex, Sym.(args)...; kwargs...)
Base.floor(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :floor)(ex, Sym.(args)...; kwargs...)
Base.product(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :product)(ex, Sym.(args)...; kwargs...)
Base.gcd(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :gcd)(ex, Sym.(args)...; kwargs...)
Base.atan(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :atan)(ex, Sym.(args)...; kwargs...)
Base.sqrt(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :sqrt)(ex, Sym.(args)...; kwargs...)
Base.acsch(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :acsch)(ex, Sym.(args)...; kwargs...)
SpecialFunctions.besselj(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :besselj)(ex, Sym.(args)...; kwargs...)
Base.adjoint(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :adjoint)(ex, Sym.(args)...; kwargs...)
Base.asinh(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :asinh)(ex, Sym.(args)...; kwargs...)
SpecialFunctions.besselk(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :besselk)(ex, Sym.(args)...; kwargs...)
Base.binomial(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :binomial)(ex, Sym.(args)...; kwargs...)
Base.asec(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :asec)(ex, Sym.(args)...; kwargs...)
Base.exp(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :exp)(ex, Sym.(args)...; kwargs...)
Base.sech(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :sech)(ex, Sym.(args)...; kwargs...)
Base.cbrt(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :cbrt)(ex, Sym.(args)...; kwargs...)
Base.acsc(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :acsc)(ex, Sym.(args)...; kwargs...)
Base.factorial(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :factorial)(ex, Sym.(args)...; kwargs...)
Base.trunc(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :trunc)(ex, Sym.(args)...; kwargs...)
Base.acos(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :acos)(ex, Sym.(args)...; kwargs...)
SpecialFunctions.polygamma(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :polygamma)(ex, Sym.(args)...; kwargs...)
Base.tanh(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :tanh)(ex, Sym.(args)...; kwargs...)
SpecialFunctions.erfinv(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :erfinv)(ex, Sym.(args)...; kwargs...)
Base.sinh(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :sinh)(ex, Sym.(args)...; kwargs...)
SpecialFunctions.airybi(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :airybi)(ex, Sym.(args)...; kwargs...)
Base.asech(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :asech)(ex, Sym.(args)...; kwargs...)
SpecialFunctions.erfcinv(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :erfcinv)(ex, Sym.(args)...; kwargs...)
SpecialFunctions.besseli(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :besseli)(ex, Sym.(args)...; kwargs...)
Base.acot(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :acot)(ex, Sym.(args)...; kwargs...)
Base.coth(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :coth)(ex, Sym.(args)...; kwargs...)
SpecialFunctions.airyai(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :airyai)(ex, Sym.(args)...; kwargs...)
SpecialFunctions.erf(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :erf)(ex, Sym.(args)...; kwargs...)
Base.cosh(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :cosh)(ex, Sym.(args)...; kwargs...)
LinearAlgebra.diag(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :diag)(ex, Sym.(args)...; kwargs...)
Base.lcm(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :lcm)(ex, Sym.(args)...; kwargs...)
Base.zeros(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :zeros)(ex, Sym.(args)...; kwargs...)
Base.cot(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :cot)(ex, Sym.(args)...; kwargs...)
SpecialFunctions.zeta(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :zeta)(ex, Sym.(args)...; kwargs...)
Base.sign(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :sign)(ex, Sym.(args)...; kwargs...)
Base.permutedims(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :permutedims)(ex, Sym.(args)...; kwargs...)
Base.cos(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :cos)(ex, Sym.(args)...; kwargs...)
Base.transpose(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :transpose)(ex, Sym.(args)...; kwargs...)
Base.MathConstants.catalan(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :catalan)(ex, Sym.(args)...; kwargs...)
SpecialFunctions.erfc(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :erfc)(ex, Sym.(args)...; kwargs...)
SpecialFunctions.bessely(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :bessely)(ex, Sym.(args)...; kwargs...)
Base.diff(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :diff)(ex, Sym.(args)...; kwargs...)
Base.tan(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :tan)(ex, Sym.(args)...; kwargs...)
Base.decompose(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :decompose)(ex, Sym.(args)...; kwargs...)
SpecialFunctions.airyaiprime(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :airyaiprime)(ex, Sym.(args)...; kwargs...)
Base.csch(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :csch)(ex, Sym.(args)...; kwargs...)
Base.csc(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :csc)(ex, Sym.(args)...; kwargs...)
Base.ones(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :ones)(ex, Sym.(args)...; kwargs...)
SpecialFunctions.trigamma(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :trigamma)(ex, Sym.(args)...; kwargs...)
Base.prod(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :prod)(ex, Sym.(args)...; kwargs...)
SpecialFunctions.beta(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :beta)(ex, Sym.(args)...; kwargs...)
Base.sec(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :sec)(ex, Sym.(args)...; kwargs...)
Base.acoth(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :acoth)(ex, Sym.(args)...; kwargs...)
SpecialFunctions.airybiprime(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :airybiprime)(ex, Sym.(args)...; kwargs...)
Base.atanh(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :atanh)(ex, Sym.(args)...; kwargs...)
LinearAlgebra.det(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :det)(ex, Sym.(args)...; kwargs...)
SpecialFunctions.gamma(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :gamma)(ex, Sym.(args)...; kwargs...)
Base.reshape(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :reshape)(ex, Sym.(args)...; kwargs...)
Base.sin(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :sin)(ex, Sym.(args)...; kwargs...)
simplify(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :simplify)(ex, Sym.(args)...; kwargs...); export simplify
summation(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :summation)(ex, Sym.(args)...; kwargs...); export summation
solve(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :solve)(ex, Sym.(args)...; kwargs...); export solve
Max(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :Max)(ex, Sym.(args)...; kwargs...); export Max
unflatten(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :unflatten)(ex, Sym.(args)...; kwargs...); export unflatten
denom(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :denom)(ex, Sym.(args)...; kwargs...); export denom
nonlinsolve(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :nonlinsolve)(ex, Sym.(args)...; kwargs...); export nonlinsolve
cancel(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :cancel)(ex, Sym.(args)...; kwargs...); export cancel
solveset(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :solveset)(ex, Sym.(args)...; kwargs...); export solveset
DiracDelta(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :DiracDelta)(ex, Sym.(args)...; kwargs...); export DiracDelta
Or(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :Or)(ex, Sym.(args)...; kwargs...); export Or
conjugate(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :conjugate)(ex, Sym.(args)...; kwargs...); export conjugate
flatten(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :flatten)(ex, Sym.(args)...; kwargs...); export flatten
Not(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :Not)(ex, Sym.(args)...; kwargs...); export Not
integrate(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :integrate)(ex, Sym.(args)...; kwargs...); export integrate
roots(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :roots)(ex, Sym.(args)...; kwargs...); export roots
factor(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :factor)(ex, Sym.(args)...; kwargs...); export factor
pdsolve(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :pdsolve)(ex, Sym.(args)...; kwargs...); export pdsolve
Abs(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :Abs)(ex, Sym.(args)...; kwargs...); export Abs
Heaviside(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :Heaviside)(ex, Sym.(args)...; kwargs...); export Heaviside
nsimplify(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :nsimplify)(ex, Sym.(args)...; kwargs...); export nsimplify
isprime(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :isprime)(ex, Sym.(args)...; kwargs...); export isprime
apart(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :apart)(ex, Sym.(args)...; kwargs...); export apart
Min(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :Min)(ex, Sym.(args)...; kwargs...); export Min
intervals(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :intervals)(ex, Sym.(args)...; kwargs...); export intervals
intersection(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :intersection)(ex, Sym.(args)...; kwargs...); export intersection
line_integrate(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :line_integrate)(ex, Sym.(args)...; kwargs...); export line_integrate
real_roots(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :real_roots)(ex, Sym.(args)...; kwargs...); export real_roots
isolate(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :isolate)(ex, Sym.(args)...; kwargs...); export isolate
linsolve(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :linsolve)(ex, Sym.(args)...; kwargs...); export linsolve
Xor(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :Xor)(ex, Sym.(args)...; kwargs...); export Xor
real_root(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :real_root)(ex, Sym.(args)...; kwargs...); export real_root
nsolve(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :nsolve)(ex, Sym.(args)...; kwargs...); export nsolve
ln(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :ln)(ex, Sym.(args)...; kwargs...); export ln
rsolve(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :rsolve)(ex, Sym.(args)...; kwargs...); export rsolve
degree(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :degree)(ex, Sym.(args)...; kwargs...); export degree
prime(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :prime)(ex, Sym.(args)...; kwargs...); export prime
limit(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :limit)(ex, Sym.(args)...; kwargs...); export limit
And(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :And)(ex, Sym.(args)...; kwargs...); export And
root(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :root)(ex, Sym.(args)...; kwargs...); export root
rootof(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :rootof)(ex, Sym.(args)...; kwargs...); export rootof
ode_order(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :ode_order)(ex, Sym.(args)...; kwargs...); export ode_order
multiplicity(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :multiplicity)(ex, Sym.(args)...; kwargs...); export multiplicity
series(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :series)(ex, Sym.(args)...; kwargs...); export series
expand(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :expand)(ex, Sym.(args)...; kwargs...); export expand
hessian(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :hessian)(ex, Sym.(args)...; kwargs...); export hessian
srepr(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :srepr)(ex, Sym.(args)...; kwargs...); export srepr
nroots(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :nroots)(ex, Sym.(args)...; kwargs...); export nroots
interpolate(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :interpolate)(ex, Sym.(args)...; kwargs...); export interpolate
numer(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :numer)(ex, Sym.(args)...; kwargs...); export numer
cse(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :cse)(ex, Sym.(args)...; kwargs...); export cse
together(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :together)(ex, Sym.(args)...; kwargs...); export together
Equality(ex::Number, args...; kwargs...)=getproperty(sympy, :Equality)(ex, Sym.(args)...; kwargs...); export Equality
Ne(ex::Number, args...; kwargs...)=getproperty(sympy, :Ne)(ex, Sym.(args)...; kwargs...); export Ne
LessThan(ex::Number, args...; kwargs...)=getproperty(sympy, :LessThan)(ex, Sym.(args)...; kwargs...); export LessThan
Gt(ex::Number, args...; kwargs...)=getproperty(sympy, :Gt)(ex, Sym.(args)...; kwargs...); export Gt
Eq(ex::Number, args...; kwargs...)=getproperty(sympy, :Eq)(ex, Sym.(args)...; kwargs...); export Eq
GreaterThan(ex::Number, args...; kwargs...)=getproperty(sympy, :GreaterThan)(ex, Sym.(args)...; kwargs...); export GreaterThan
Le(ex::Number, args...; kwargs...)=getproperty(sympy, :Le)(ex, Sym.(args)...; kwargs...); export Le
Lt(ex::Number, args...; kwargs...)=getproperty(sympy, :Lt)(ex, Sym.(args)...; kwargs...); export Lt
Unequality(ex::Number, args...; kwargs...)=getproperty(sympy, :Unequality)(ex, Sym.(args)...; kwargs...); export Unequality
Ge(ex::Number, args...; kwargs...)=getproperty(sympy, :Ge)(ex, Sym.(args)...; kwargs...); export Ge
StrictLessThan(ex::Number, args...; kwargs...)=getproperty(sympy, :StrictLessThan)(ex, Sym.(args)...; kwargs...); export StrictLessThan
StrictGreaterThan(ex::Number, args...; kwargs...)=getproperty(sympy, :StrictGreaterThan)(ex, Sym.(args)...; kwargs...); export StrictGreaterThan
