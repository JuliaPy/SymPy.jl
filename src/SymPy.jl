module SymPy

## SymPy package to interface with Python's [SymPy package](http://www.sympy.org) through `PyCall`
##
## The basic idea is that a new type `Sym` is made to hold symbolic objects.
##
## For this type, the basic operators and appropriate functions of `Julia` are overloaded for `Sym` objects.
##
## As well, many -- but not all -- of the SymPy functions are ported
## to allow them to be called on `Sym` objects. For documentation, one
## should refer to [SymPy's
## website](http://docs.sympy.org/latest/index.html).



using PyCall
@pyimport sympy


using Requires ## for @require macro

## * Docile is used for documentation
if VERSION < v"0.4.0-dev"
    using Docile
else
    macro document() nothing end    
end
@document

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
import Base: factorial, gcd, lcm, isqrt
import Base: gamma, beta
import Base: length,  size
import Base: factor, expand, collect
import Base: !=, ==
import Base:  LinAlg.det, LinAlg.inv, LinAlg.conj,
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

## conditional imports
if VERSION < v"0.4.0-dev"
    import Base: rref
end

export sympy, sympy_meth, object_meth, call_matrix_meth
export Sym, @sym_str, @syms, symbols
export pprint,  jprint
export SymFunction, SymMatrix,
       n,  subs,
       simplify, nsimplify, 
       expand, factor, trunc,
       collect, separate, 
       fraction,
       primitive, sqf, resultant, cancel,
       together, square,
       solve,
       limit, diff, 
       series, integrate, 
       summation,
       I, oo,
       dsolve,
       poly,  nroots, real_roots,
       ∨, ∧
export relation, piecewise
export members, doc, _str

export PI, E

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
                  polynomial_instance_methods)

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
