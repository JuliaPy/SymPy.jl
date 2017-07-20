__precompile__()

## TODO:
## * tidy up code

module SymPy

using Compat
import Compat: @compat, invokelatest

"""

SymPy package to interface with Python's [SymPy package](http://www.sympy.org) through `PyCall`.

The basic idea is that a new type -- `Sym` -- is made to hold symbolic
objects.  For this type, the basic operators and appropriate functions
of `Julia` are overloaded for `Sym` objects so that the expressions
are treated symbolically and not evaluated immediately. Instances of
this type are created by the constructor `Sym` or `symbols` or the macro
`@vars`.

As well, many -- but not all -- of the SymPy functions are ported to
allow them to be called on `Sym` objects. Mostly these are implemented
through metaprogramming, so adding missing functions is not hard. They
are not generated automatically though, rather added by hand.

To find documentation on SymPy functions, one should refer to
SymPy's [website](http://docs.sympy.org/latest/index.html).

Plotting is provided through the `Plots` interface. For details, see the help page for `sympy_plotting`.

The package tutorial provides many examples. This can be read on
[GitHub](https://github.com/jverzani/SymPy.jl/blob/master/examples/tutorial.ipynb).

"""
SymPy


using Compat

using PyCall

import Base: show
import Base: convert, promote_rule
import Base: getindex
import Base: start, next, done
import Base: complex
import Base: sin, cos, tan, sinh, cosh, tanh, asin, acos,
       atan, asinh, acosh, atanh, sec, csc, cot, asec,
       acsc, acot, sech, csch, coth, asech, acsch, acoth,
       sinc, cosc, cosd, cotd, cscd, secd, sind, tand,
       acosd, acotd, acscd, asecd, asind, atand, atan2,
       sinpi, cospi,
       log, log2,
       log10, log1p, exponent, exp, exp2, expm1, cbrt, sqrt,
       erf, erfc, erfcx, erfi, erfinv, erfcinv, dawson, ceil, floor,
       trunc, round, significand,
       abs, max, min, maximum, minimum,
       sign, dot,
       zero, one,
       hypot
import Base: transpose
import Base: diff
import Base: factorial, gcd, lcm, isqrt
import Base: length,  size
import Base: expand, collect
import Base: !=, ==
import Base:  inv, conj, det,
              cross, eigvals, eigvecs, trace, norm, chol
import Base: promote_rule
import Base: match, replace, round
import Base: +, -, *, /, //, \
import Base: ^, .^
import Base: &, |, !, >, >=, ==, <=, <
## poly.jl
import Base: div
import Base: trunc
import Base: isinf, isnan
import Base: real, imag
import Base: expm
import Base: nullspace

if VERSION < v"0.6.0-dev"
    import Base: factor, isprime
end

export sympy, sympy_meth, @sympy_str, object_meth, call_matrix_meth
export Sym, @syms, @vars, symbols
export pprint,  jprint
export SymFunction, @symfuns,
       SymMatrix,
       evalf, N,  subs,
       simplify, nsimplify,
       expand, factor, trunc,
       collect, separate,
       fraction,
       primitive, sqf, resultant, cancel,
       together,
       solve,
       limit,
       series, integrate,
       summation,
       dsolve,
       poly,  nroots, real_roots, polyroots,
       ∨, ∧, ¬,
       rhs, lhs, args,
       jacobian, hessian,
       Max, Min,
       rref
export PI, E, IM, oo
export relation, piecewise, Piecewise, piecewise_fold
export members, doc, _str


## Following PyPlot, we initialize our variables outside _init_
const sympy  = PyCall.PyNULL()
const mpmath = PyCall.PyNULL()


include("types.jl")
include("utils.jl")
include("mathops.jl")
include("core.jl")
include("logical.jl")
include("math.jl")
include("mpmath.jl")
include("specialfuns.jl")
include("solve.jl")
include("dsolve.jl")
include("subs.jl")
include("patternmatch.jl")
include("simplify.jl")
include("series.jl")
include("integrate.jl")
include("assumptions.jl")
include("poly.jl")
include("matrix.jl")
include("ntheory.jl")
include("sets.jl")
include("display.jl")
include("lambdify.jl")
include("physics.jl")

## add call interface depends on version
VERSION >= v"0.5.0-" && include("call.jl")
v"0.4.0" <= VERSION < v"0.5.0-" && include("call-0.4.jl")

include("plot_recipes.jl") # hook into Plots

## create some methods

## These are base methods, so imported. Important  that first argument is Sym class for dispatch
for meth in union(
                  math_sympy_methods_base,
                  polynomial_sympy_methods_base

                  )

    meth_name = string(meth)
#    eval(Expr(:import, :Base, meth)) # (kept in import list above)
    @eval begin
                @doc """
`$($meth_name)`: a SymPy function.
The SymPy documentation can be found through: http://docs.sympy.org/latest/search.html?q=$($meth_name)
    """ ->

        ($meth)(ex::Sym, args...; kwargs...) =
            sympy_meth($meth_name, ex, args...; kwargs...)
    end
end

## These are *added*, so exported
for meth in union(core_sympy_methods,
                  math_sympy_methods,
                  simplify_sympy_meths,
                  expand_sympy_meths,
                  functions_sympy_methods,
                  series_sympy_meths,
                  integrals_sympy_methods,
                  logical_sympy_methods,
                  summations_sympy_methods,
                  logic_sympy_methods,
                  polynomial_sympy_methods,
                  ntheory_sympy_methods,
                  solveset_sympy_methods
                  )

    meth_name = string(meth)
    @eval begin
        @doc """
`$($meth_name)`: a SymPy function.
The SymPy documentation can be found through: http://docs.sympy.org/latest/search.html?q=$($meth_name)
""" ->
        ($meth){T<:SymbolicObject}(ex::T, args...; kwargs...) = sympy_meth($meth_name, ex, args...; kwargs...)

    end
    eval(Expr(:export, meth))
end


## Thse are object methods that need importing
for meth in union(math_object_methods_base,
                  series_object_meths_base
                  )

    meth_name = string(meth)
    @eval begin
        @doc """
`$($meth_name)`: a SymPy function.
The SymPy documentation can be found through: http://docs.sympy.org/latest/search.html?q=$($meth_name)
""" ->
        ($meth)(ex::SymbolicObject, args...; kwargs...) = object_meth(ex, $meth_name, args...; kwargs...)
    end
end

## Thse are object methods that need exporting
for meth in union(core_object_methods,
                  integrals_instance_methods,
                  summations_instance_methods,
                  polynomial_instance_methods,
                  series_object_meths
                  )

    meth_name = string(meth)
    @eval begin
        @doc """
`$($meth_name)`: a SymPy function.
The SymPy documentation can be found through: http://docs.sympy.org/latest/search.html?q=$($meth_name)
""" ->
        ($meth)(ex::SymbolicObject, args...; kwargs...) = object_meth(ex, $meth_name, args...; kwargs...)
    end
    eval(Expr(:export, meth))
end


# These are object properties
for prop in union(core_object_properties,
                  summations_object_properties,
                  polynomial_predicates)

    prop_name = string(prop)
    @eval ($prop)(ex::Sym) = PyObject(ex)[@compat(Symbol($prop_name))]
    eval(Expr(:export, prop))
end



## Makes it possible to call in a sympy method, witout worrying about Sym objects

global call_sympy_fun(fn::Function, args...; kwargs...) = fn(args...; kwargs...)

global call_sympy_fun(fn::PyCall.PyObject, args...; kwargs...) = PyCall.pycall(fn, PyAny, args...; kwargs...)

## Main interface to methods in sympy
## sympy_meth(:name, ars, kwars...)
global sympy_meth(meth, args...; kwargs...) = begin
    ans = call_sympy_fun(sympy[string(meth)], args...; kwargs...)
    ## make nicer...
    try
        if isa(ans, Vector)
            ans = Sym[i for i in ans]
        end
    catch err
    end
    ans
end


# """

#    sympy"fn_name"(args...; kwargs...)

# Call a SymPy method using a string macro. The value returned by `sympy"fn_name"` is a function
# that calls into SymPy via PyCall. This just wraps `sympy_meth`.

# Examples:
# ```
# @vars x
# sympy"integrate"(x^2, (x, 0, 1))
# ```
# """

sympy_str_v0_5 = quote
macro sympy_str(s)
    (args...; kwargs...) -> _sympy_str(Symbol(s), args...; kwargs...)
end
end
sympy_str_v0_4 = quote
macro sympy_str(s)
    (args...) -> _sympy_str(Symbol(s), args...)
end
end    
VERSION < v"0.5.0" ? eval(sympy_str_v0_4) : eval(sympy_str_v0_5)

"""

    try various ways of calling a sympy function specified as a key

function behind `sympy"key"(...)` interface        

"""
function _sympy_str(fn, args...; kwargs...)
    try
        sympy_meth(Symbol(fn), args...; kwargs...)
    catch err
        try
            xs = [args...]
            x = shift!(xs)
            object_meth(x, fn, xs...; kwargs...)
        catch err
            try
                xs = [args...]
                x = shift!(xs)
                call_matrix_meth(x, fn, xs...; kwargs...)
            catch err
                try
                    mpmath_meth(fn, args...; kwargs...)
                catch err
                    throw(ArgumentError("Can not find this method $fn for the given signature"))
                end
            end
        end
    end
end


global object_meth(object::SymbolicObject, meth, args...; kwargs...)  =  begin
    call_sympy_fun(PyObject(object)[@compat(Symbol(meth))],  args...; kwargs...)

end



## For precompilation we must put PyCall instances in __init__:
function __init__()

    ## Define sympy, mpmath, ...
    copy!(sympy, PyCall.pyimport_conda("sympy", "sympy"))

    ## mappings from PyObjects to types.
    basictype = sympy["basic"]["Basic"]
    pytype_mapping(basictype, Sym)

    polytype = sympy["polys"]["polytools"]["Poly"]
    pytype_mapping(polytype, Sym)

    try
        matrixtype = sympy["matrices"]["MatrixBase"]
        pytype_mapping(matrixtype, Array{Sym})
        pytype_mapping(sympy["Matrix"], Array{Sym})        
    catch e
    end

    ##
    init_logical()
    init_math()
    init_mpmath()
    init_sets()
    init_lambdify()
    init_physics()
end

end
