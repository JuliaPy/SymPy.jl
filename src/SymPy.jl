"""

`SymPy` package to interface with Python's [SymPy library](http://www.sympy.org) through `PyCall`.

The basic idea is that a new type -- `Sym` -- is made to hold symbolic
objects.  For this type, the basic functions from SymPy and appropriate functions
of `Julia` are overloaded for `Sym` objects so that the expressions
are treated symbolically and not evaluated immediately. Instances of
this type are created by the constructor `Sym`, the function `symbols` or the macro
`@vars`.

On loading, a priviledged set of the functions from the `sympy` module
are defined as generic functions with their first argument narrowed to
symbolic types. Others may be accessed by qualification, as in
`sympy.trigsimp`. Calling `import_from(sympy)` will import the
rest. SymPy methods are called through Python's dot-call syntax.  To
find documentation on SymPy functions and methods, one should refer to
SymPy's [website](http://docs.sympy.org/latest/index.html).

Plotting is provided through the `Plots` interface. For details, see
the help page for `sympy_plotting`.

The package tutorial provides many examples. This can be read on
[GitHub](http://nbviewer.ipython.org/github/JuliaPy/SymPy.jl/blob/master/examples/tutorial.ipynb).

"""
module SymPy


## minimal version of conveniently using SymPy in Julia
## uses getfield overloading to access sympy methods of Sym object

using PyCall

using SpecialFunctions
using LinearAlgebra
using OffsetArrays


import Base: show
import Base: convert, promote_rule
import Base: getproperty
import Base: hash, ==
import Base: length, size
import Base.iterate
import Base: +, -, *, /, //, \, ^

export @vars, Sym, sympify, symbols, @symfuns, @syms
export SymMatrix, SymFunction
export PI, IM, oo, zoo, True, False
export N, subs

export sympy, import_from#, import_sympy
export free_symbols


include("types.jl")
include("constructors.jl")
include("utils.jl")
include("numbers.jl")
include("mathops.jl")
include("mathfuns.jl")
include("generic.jl")
include("matrix.jl")
include("sets.jl")
include("symfunction.jl")
include("assumptions.jl")
include("lambdify.jl")
include("patternmatch.jl")
include("permutations.jl")
include("plot_recipes.jl")

##################################################

pynull() = PyCall.PyNULL()
const sympy  = PyCall.PyNULL()
const mpmath = PyCall.PyNULL()
const combinatorics  = PyCall.PyNULL()

# core.sympy.numbers.*
"PI is symbolic `pi`"
global PI = Sym(pynull())
"IM is a symbolic `im`"
global IM = Sym(pynull())
"oo is a symbolic infinity. Example: `integrate(exp(-x), x, 0, oo)`."
global oo = Sym(pynull())
"zoo complex inifinity"
global zoo = Sym(pynull())
"True from SymPy"
global True = Sym(pynull())
"False from SymPy"
global False = Sym(pynull())



# Can not actually initiate many things until `sympy` is defined, so not until runtime
function __init__()

    ## Define sympy, mpmath, ...
    copy!(sympy, PyCall.pyimport_conda("sympy", "sympy"))
    copy!(PI.x,  sympy.pi)
    copy!(IM.x, sympy.I)
    copy!(oo.x, sympy.oo)
    copy!(zoo.x, sympy.zoo)
    copy!(True.x, PyCall.PyObject(true))
    copy!(False.x, PyCall.PyObject(false))


    # mpmath
    try
        PyCall.mpmath_init()
        copy!(mpmath, PyCall.pyimport_conda("mpmath", "mpmath"))
    catch err
       # can't load
    end

    # pull in alibrary
    copy!(combinatorics, PyCall.pyimport_conda("sympy.combinatorics", "sympy"))


    ## mappings from PyObjects to types.
    ## order here is fussy, as we needed ImmutableMatrix before Basic
    pytype_mapping(sympy.ImmutableMatrix, SymMatrix)
    pytype_mapping(sympy.ImmutableDenseMatrix, SymMatrix)

    basictype = sympy.basic.Basic
    pytype_mapping(basictype, Sym)

    pytype_mapping(sympy.Matrix, Array{Sym})
    pytype_mapping(sympy.matrices.MatrixBase, Array{Sym})

    pytype_mapping(sympy.FiniteSet, Set)

    pytype_mapping(sympy.combinatorics.permutations.Permutation, SymPermutation)
    pytype_mapping(combinatorics.perm_groups.PermutationGroup, SymPermutationGroup)

    if mpmath != PyCall.PyNULL()
        ## ignore warnings for now...
        mpftype = mpmath."mpf"
        pytype_mapping(mpftype, BigFloat) ## Raises warning!!!
        mpctype = mpmath."mpc"
        pytype_mapping(mpctype, Complex{BigFloat})
    end



    ## is this a good idea?
    ## could leave this out
    ## leave out except when generating the list below (cf. utils.jl for println statements)
    #import_sympy()

end


## On load, generic functions in the base modules that match a function in sympy.* are
## defined on `Sym` objects, as are those listed here:
##
priviledged = (:And, :Or, :Not, :Xor,
               #
               :apart, :cancel, :cse, :expand, :factor, :flatten, :nsimplify,
               :isolate, :simplify, :together, :unflatten,
               #
               :srepr,:doit,
               #
               :integrate, :line_integrate, :interpolate, :limit, :series, :summation,
               :hessian,
               #
               :prime, :multiplicity, :degree, :coeffs,
               #
               :DiracDelta, :Heaviside,
               #
               :linsolve, :nonlinsolve, :nroots, :nsolve, :pdsolve, :real_root,
               :real_roots, :root, :rootof, :roots, :rsolve, :solve, :solveset,
               :ode_order,
               #
               :Min, :Max, :Abs,:numer, :denom, :conjugate, :ln,
               #
               :intersection, :intervals, :isprime

               )


function in_base(uv)
    u = Symbol(uv[1])
    for M in base_Ms
        isdefined(M, u) && return true
    end
    false
end
is_function(uv) = SymPy.pycall_hasproperty(uv[2], :__class__) &&  occursin("unction", uv[2].__class__.__name__)



"""
    import_sympy

This method imports all functions from `mpmath` and a priviledged set
of functions from `sympy`, as well as the relational operators.

These functions are narrowed on their first argument being of type `SymbolicObject`.

A few modules are checked for namespace collisions.

If a function naturally takes an non-Symbolic argument as a first argument, then it can be qualified: e.g.
`sympy.sin(2)` (as opposed to `sin(Sym(2))`).

If a constructor is needed (which is not a function) then it must be
qualified. (E.g. `sympy.Function("F")`, though for this particular
case, there is `SymFunction` defined for convenience.)

"""
function import_sympy()
    if mpmath != PyCall.PyNULL()
        mps = _get_member_functions(mpmath, base_exclude)
        sps = Introspection.getmembers(sympy)
        imp = Tuple(Symbol.(setdiff(keys(mps), keys(sps))))
        import_from(mpmath, imp)
    end
    ## import from
    ## import_from(sympy)
    d = Introspection.getmembers(sympy)
    d1 = filter(uv -> in_base(uv) && is_function(uv), d)
    import_from(sympy, setdiff(Symbol.(collect(keys(d1))),  Symbol.(base_exclude)))
    import_from(sympy, priviledged)
    import_from(sympy, (:Ne,  :Le, :Eq, :Ge, :Gt,
                        :GreaterThan, :LessThan,
                        :StrictGreaterThan, :StrictLessThan,
                        :Equality, :Unequality
                        ), typ=:Number)
end

## :Lt is in Base.Order
import Base.Order: Lt
Lt(x::Number, args...;kwargs...) = sympy.Lt(x, args...; kwargs...)
export(Lt)


###
### These are generated by a) uncommenting import_sypmy() in __init__ b) uncomment println parts in import_from
expj(ex::SymbolicObject, args...; kwargs...)=getproperty(mpmath, :expj)(ex, args...; kwargs...); export expj
fac(ex::SymbolicObject, args...; kwargs...)=getproperty(mpmath, :fac)(ex, args...; kwargs...); export fac
nint(ex::SymbolicObject, args...; kwargs...)=getproperty(mpmath, :nint)(ex, args...; kwargs...); export nint
Base.ceil(ex::SymbolicObject, args...; kwargs...)=getproperty(mpmath, :ceil)(ex, args...; kwargs...)
fib(ex::SymbolicObject, args...; kwargs...)=getproperty(mpmath, :fib)(ex, args...; kwargs...); export fib
monitor(ex::SymbolicObject, args...; kwargs...)=getproperty(mpmath, :monitor)(ex, args...; kwargs...); export monitor
Base.cospi(ex::SymbolicObject, args...; kwargs...)=getproperty(mpmath, :cospi)(ex, args...; kwargs...)
bernfrac(ex::SymbolicObject, args...; kwargs...)=getproperty(mpmath, :bernfrac)(ex, args...; kwargs...); export bernfrac
doctests(ex::SymbolicObject, args...; kwargs...)=getproperty(mpmath, :doctests)(ex, args...; kwargs...); export doctests
ei(ex::SymbolicObject, args...; kwargs...)=getproperty(mpmath, :ei)(ex, args...; kwargs...); export ei
Base.sinpi(ex::SymbolicObject, args...; kwargs...)=getproperty(mpmath, :sinpi)(ex, args...; kwargs...)
timing(ex::SymbolicObject, args...; kwargs...)=getproperty(mpmath, :timing)(ex, args...; kwargs...); export timing
rgamma(ex::SymbolicObject, args...; kwargs...)=getproperty(mpmath, :rgamma)(ex, args...; kwargs...); export rgamma
expjpi(ex::SymbolicObject, args...; kwargs...)=getproperty(mpmath, :expjpi)(ex, args...; kwargs...); export expjpi
ellipk(ex::SymbolicObject, args...; kwargs...)=getproperty(mpmath, :ellipk)(ex, args...; kwargs...); export ellipk
e1(ex::SymbolicObject, args...; kwargs...)=getproperty(mpmath, :e1)(ex, args...; kwargs...); export e1
SpecialFunctions.digamma(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :digamma)(ex, args...; kwargs...)
Base.rem(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :rem)(ex, args...; kwargs...)
Base.asin(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :asin)(ex, args...; kwargs...)
Base.acosh(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :acosh)(ex, args...; kwargs...)
Base.im(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :im)(ex, args...; kwargs...)
Base.collect(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :collect)(ex, args...; kwargs...)
Base.Function(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :Function)(ex, args...; kwargs...)
SpecialFunctions.erfi(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :erfi)(ex, args...; kwargs...)
Base.floor(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :floor)(ex, args...; kwargs...)
Base.product(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :product)(ex, args...; kwargs...)
Base.gcd(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :gcd)(ex, args...; kwargs...)
Base.atan(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :atan)(ex, args...; kwargs...)
Base.sqrt(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :sqrt)(ex, args...; kwargs...)
Base.acsch(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :acsch)(ex, args...; kwargs...)
SpecialFunctions.besselj(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :besselj)(ex, args...; kwargs...)
Base.adjoint(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :adjoint)(ex, args...; kwargs...)
Base.asinh(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :asinh)(ex, args...; kwargs...)
SpecialFunctions.besselk(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :besselk)(ex, args...; kwargs...)
Base.binomial(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :binomial)(ex, args...; kwargs...)
Base.asec(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :asec)(ex, args...; kwargs...)
Base.exp(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :exp)(ex, args...; kwargs...)
Base.sech(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :sech)(ex, args...; kwargs...)
Base.cbrt(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :cbrt)(ex, args...; kwargs...)
Base.acsc(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :acsc)(ex, args...; kwargs...)
Base.factorial(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :factorial)(ex, args...; kwargs...)
Base.trunc(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :trunc)(ex, args...; kwargs...)
Base.acos(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :acos)(ex, args...; kwargs...)
SpecialFunctions.polygamma(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :polygamma)(ex, args...; kwargs...)
Base.tanh(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :tanh)(ex, args...; kwargs...)
SpecialFunctions.erfinv(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :erfinv)(ex, args...; kwargs...)
Base.sinh(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :sinh)(ex, args...; kwargs...)
SpecialFunctions.airybi(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :airybi)(ex, args...; kwargs...)
Base.asech(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :asech)(ex, args...; kwargs...)
SpecialFunctions.erfcinv(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :erfcinv)(ex, args...; kwargs...)
SpecialFunctions.besseli(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :besseli)(ex, args...; kwargs...)
Base.acot(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :acot)(ex, args...; kwargs...)
Base.coth(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :coth)(ex, args...; kwargs...)
SpecialFunctions.airyai(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :airyai)(ex, args...; kwargs...)
SpecialFunctions.erf(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :erf)(ex, args...; kwargs...)
Base.cosh(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :cosh)(ex, args...; kwargs...)
LinearAlgebra.diag(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :diag)(ex, args...; kwargs...)
Base.lcm(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :lcm)(ex, args...; kwargs...)
Base.zeros(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :zeros)(ex, args...; kwargs...)
Base.cot(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :cot)(ex, args...; kwargs...)
SpecialFunctions.zeta(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :zeta)(ex, args...; kwargs...)
Base.sign(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :sign)(ex, args...; kwargs...)
Base.permutedims(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :permutedims)(ex, args...; kwargs...)
Base.cos(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :cos)(ex, args...; kwargs...)
Base.transpose(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :transpose)(ex, args...; kwargs...)
Base.MathConstants.catalan(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :catalan)(ex, args...; kwargs...)
SpecialFunctions.erfc(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :erfc)(ex, args...; kwargs...)
SpecialFunctions.bessely(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :bessely)(ex, args...; kwargs...)
Base.diff(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :diff)(ex, args...; kwargs...)
Base.tan(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :tan)(ex, args...; kwargs...)
Base.decompose(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :decompose)(ex, args...; kwargs...)
SpecialFunctions.airyaiprime(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :airyaiprime)(ex, args...; kwargs...)
Base.csch(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :csch)(ex, args...; kwargs...)
Base.csc(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :csc)(ex, args...; kwargs...)
Base.ones(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :ones)(ex, args...; kwargs...)
SpecialFunctions.trigamma(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :trigamma)(ex, args...; kwargs...)
Base.prod(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :prod)(ex, args...; kwargs...)
SpecialFunctions.beta(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :beta)(ex, args...; kwargs...)
Base.sec(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :sec)(ex, args...; kwargs...)
Base.acoth(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :acoth)(ex, args...; kwargs...)
SpecialFunctions.airybiprime(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :airybiprime)(ex, args...; kwargs...)
Base.atanh(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :atanh)(ex, args...; kwargs...)
LinearAlgebra.det(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :det)(ex, args...; kwargs...)
SpecialFunctions.gamma(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :gamma)(ex, args...; kwargs...)
Base.reshape(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :reshape)(ex, args...; kwargs...)
Base.sin(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :sin)(ex, args...; kwargs...)
simplify(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :simplify)(ex, args...; kwargs...); export simplify
summation(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :summation)(ex, args...; kwargs...); export summation
solve(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :solve)(ex, args...; kwargs...); export solve
Max(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :Max)(ex, args...; kwargs...); export Max
unflatten(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :unflatten)(ex, args...; kwargs...); export unflatten
denom(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :denom)(ex, args...; kwargs...); export denom
nonlinsolve(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :nonlinsolve)(ex, args...; kwargs...); export nonlinsolve
cancel(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :cancel)(ex, args...; kwargs...); export cancel
solveset(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :solveset)(ex, args...; kwargs...); export solveset
DiracDelta(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :DiracDelta)(ex, args...; kwargs...); export DiracDelta
Or(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :Or)(ex, args...; kwargs...); export Or
conjugate(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :conjugate)(ex, args...; kwargs...); export conjugate
flatten(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :flatten)(ex, args...; kwargs...); export flatten
Not(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :Not)(ex, args...; kwargs...); export Not
integrate(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :integrate)(ex, args...; kwargs...); export integrate
roots(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :roots)(ex, args...; kwargs...); export roots
factor(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :factor)(ex, args...; kwargs...); export factor
pdsolve(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :pdsolve)(ex, args...; kwargs...); export pdsolve
Abs(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :Abs)(ex, args...; kwargs...); export Abs
Heaviside(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :Heaviside)(ex, args...; kwargs...); export Heaviside
nsimplify(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :nsimplify)(ex, args...; kwargs...); export nsimplify
isprime(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :isprime)(ex, args...; kwargs...); export isprime
apart(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :apart)(ex, args...; kwargs...); export apart
Min(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :Min)(ex, args...; kwargs...); export Min
intervals(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :intervals)(ex, args...; kwargs...); export intervals
intersection(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :intersection)(ex, args...; kwargs...); export intersection
line_integrate(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :line_integrate)(ex, args...; kwargs...); export line_integrate
real_roots(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :real_roots)(ex, args...; kwargs...); export real_roots
isolate(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :isolate)(ex, args...; kwargs...); export isolate
linsolve(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :linsolve)(ex, args...; kwargs...); export linsolve
Xor(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :Xor)(ex, args...; kwargs...); export Xor
real_root(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :real_root)(ex, args...; kwargs...); export real_root
nsolve(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :nsolve)(ex, args...; kwargs...); export nsolve
ln(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :ln)(ex, args...; kwargs...); export ln
rsolve(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :rsolve)(ex, args...; kwargs...); export rsolve
degree(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :degree)(ex, args...; kwargs...); export degree
prime(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :prime)(ex, args...; kwargs...); export prime
limit(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :limit)(ex, args...; kwargs...); export limit
And(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :And)(ex, args...; kwargs...); export And
root(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :root)(ex, args...; kwargs...); export root
rootof(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :rootof)(ex, args...; kwargs...); export rootof
ode_order(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :ode_order)(ex, args...; kwargs...); export ode_order
multiplicity(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :multiplicity)(ex, args...; kwargs...); export multiplicity
series(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :series)(ex, args...; kwargs...); export series
expand(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :expand)(ex, args...; kwargs...); export expand
hessian(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :hessian)(ex, args...; kwargs...); export hessian
srepr(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :srepr)(ex, args...; kwargs...); export srepr
nroots(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :nroots)(ex, args...; kwargs...); export nroots
interpolate(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :interpolate)(ex, args...; kwargs...); export interpolate
numer(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :numer)(ex, args...; kwargs...); export numer
cse(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :cse)(ex, args...; kwargs...); export cse
together(ex::SymbolicObject, args...; kwargs...)=getproperty(sympy, :together)(ex, args...; kwargs...); export together
Equality(ex::Number, args...; kwargs...)=getproperty(sympy, :Equality)(ex, args...; kwargs...); export Equality
Ne(ex::Number, args...; kwargs...)=getproperty(sympy, :Ne)(ex, args...; kwargs...); export Ne
LessThan(ex::Number, args...; kwargs...)=getproperty(sympy, :LessThan)(ex, args...; kwargs...); export LessThan
Gt(ex::Number, args...; kwargs...)=getproperty(sympy, :Gt)(ex, args...; kwargs...); export Gt
Eq(ex::Number, args...; kwargs...)=getproperty(sympy, :Eq)(ex, args...; kwargs...); export Eq
GreaterThan(ex::Number, args...; kwargs...)=getproperty(sympy, :GreaterThan)(ex, args...; kwargs...); export GreaterThan
Le(ex::Number, args...; kwargs...)=getproperty(sympy, :Le)(ex, args...; kwargs...); export Le
Unequality(ex::Number, args...; kwargs...)=getproperty(sympy, :Unequality)(ex, args...; kwargs...); export Unequality
Ge(ex::Number, args...; kwargs...)=getproperty(sympy, :Ge)(ex, args...; kwargs...); export Ge
StrictLessThan(ex::Number, args...; kwargs...)=getproperty(sympy, :StrictLessThan)(ex, args...; kwargs...); export StrictLessThan
StrictGreaterThan(ex::Number, args...; kwargs...)=getproperty(sympy, :StrictGreaterThan)(ex, args...; kwargs...); export StrictGreaterThan

end # module
