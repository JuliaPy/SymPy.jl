module SymPy


"""
SymPy package to interface with Python's [SymPy package](http://www.sympy.org) through `PyCall`.

The basic idea is that a new type `Sym` is made to hold symbolic
objects.  For this type, the basic operators and appropriate functions
of `Julia` are overloaded for `Sym` objects so that the expressions
are treated symbolically and not evaluated immediately.

As well, many -- but not all -- of the SymPy functions are ported to
allow them to be called on `Sym` objects. Mostly these are implemented
through metaprogramming, so adding missing functions is not hard.

For documentation on SymPy functions, one should refer to 
SymPy's [website](http://docs.sympy.org/latest/index.html).

The package tutorial provides many examples.
"""
SymPy


using Compat

using PyCall
@pyimport sympy
## how to check if symbol in module more quickly?
if :mpmath in names(sympy)
    @pyimport sympy.mpmath as mpmath
else
    @pyimport mpmath
end

using Requires ## for @require macro

## * Docile is used for documentation
if VERSION < v"0.4.0-dev"
    using Docile
else
    macro document() nothing end    
end
@document
#@docstrings

import Base: show, writemime
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
       radians2degrees, degrees2radians, log, log2,
       log10, log1p, exponent, exp, exp2, expm1, cbrt, sqrt,
       erf, erfc, erfcx, erfi, dawson, ceil, floor,
       trunc, round, significand,
       abs, max, min, maximum, minimum,
       sign, dot,
       besseli, besselj, besselk, bessely,
       airyai, airybi,
       zero, one
import Base: transpose
import Base: diff       
import Base: factorial, gcd, lcm, isqrt
import Base: gamma, beta
import Base: length,  size
import Base: factor, expand, collect
import Base: !=, ==
import Base:  inv, conj,
              cross, eigvals, eigvecs, trace, norm
import Base: promote_rule
import Base: match, replace, round
import Base: ^, .^
import Base: &, |, !, >, >=, ==, <=, <
## poly.jl
import Base: div
import Base: trunc
import Base: isinf, isnan
import Base: real, imag
import Base: expm

## conditional imports
if VERSION < v"0.4.0-dev"
    import Base: rref, det
end

export sympy, sympy_meth, object_meth, call_matrix_meth
export Sym, @syms, @vars, symbols # @sym_str,
export pprint,  jprint
export SymFunction, SymMatrix,
       evalf, N,  subs,
       simplify, nsimplify, 
       expand, factor, trunc,
       collect, separate, 
       fraction,
       primitive, sqf, resultant, cancel,
       together, square,
       solve,
       limit,
       series, integrate, 
       summation,
       dsolve,
       poly,  nroots, real_roots, polyroots,
       ∨, ∧, ¬,
       rhs, lhs, args
export relation, piecewise
export members, doc, _str

export PI, E, IM, oo

include("types.jl")
include("utils.jl")
include("mathops.jl")
include("core.jl")
include("logical.jl")
include("math.jl")
include("simplify.jl")
include("functions.jl")
include("series.jl")
include("integrate.jl")
include("assumptions.jl")
include("poly.jl")
include("matrix.jl")
include("ntheory.jl")
include("display.jl")
include("plot.jl")


## create some methods

for meth in union(core_sympy_methods,
                  simplify_sympy_meths,
                  expand_sympy_meths,
                  functions_sympy_methods,
                  series_sympy_meths,
                  integrals_sympy_methods,
                  summations_sympy_methods,
                  logic_sympy_methods,
                  polynomial_sympy_methods,
                  ntheory_sympy_methods
                  )

    meth_name = string(meth)
    @eval ($meth)(ex::Sym, args...; kwargs...) = sympy_meth(symbol($meth_name), ex, args...; kwargs...)
    eval(Expr(:export, meth))
end


for meth in union(core_object_methods,
                  integrals_instance_methods,
                  summations_instance_methods,
                  polynomial_instance_methods
                  )

    meth_name = string(meth)
    @eval ($meth)(ex::Sym, args...; kwargs...) = object_meth(ex, symbol($meth_name), args...; kwargs...)
    eval(Expr(:export, meth))
end



for prop in union(core_object_properties,
                  summations_object_properties,
                  polynomial_predicates)
    
    prop_name = string(prop)
    @eval ($prop)(ex::Sym) = ex[symbol($prop_name)]
    eval(Expr(:export, prop))
end


end
