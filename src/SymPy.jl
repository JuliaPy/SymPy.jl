module SymPy

using PyCall
@pyimport sympy
using GoogleCharts

import Base.getindex
import Base: show, repl_show
import Base.convert, Base.complex
import Base: sin, cos, tan, sinh, cosh, tanh, asin, acos,
       atan, asinh, acosh, atanh, sec, csc, cot, asec,
       acsc, acot, sech, csch, coth, asech, acsch, acoth,
       sinc, cosc, cosd, cotd, cscd, secd, sind, tand,
       acosd, acotd, acscd, asecd, asind, atand, atan2,
       radians2degrees, degrees2radians, log, log2,
       log10, log1p, exponent, exp, exp2, expm1, cbrt, sqrt,
       square, erf, erfc, erfcx, erfi, dawson, ceil, floor,
       trunc, round, significand,
       abs
import Base: factorial, gcd, lcm, isqrt
import Base: gamma, beta
import Base: solve, length,  size
import Base: factor, expand
import Base: !=, ==
import Base:  LinAlg.det, LinAlg.inv, LinAlg.conj,
              cross, eigvals, eigvecs, rref, trace
import Base: promote_rule
import Base: has, match, replace, round
## poly.jl
import Base: div

export sympy, sympy_meth, object_meth, call_matrix_meth
export Sym, @sym_str, @syms, symbols
export pprint, latex, jprint
export SymFunction, SymMatrix,
       n,  subs,
       simplify, nsimplify, 
       expand, factor, trunc,
       collect, separate, 
       fraction,
       primitive, sqf, resultant, cancel,
       expand, together,
       limit, diff, 
       series, integrate, 
       summation,
       I, oo,
       Ylm, assoc_legendre, chebyshevt, legendre, hermite,
       dsolve,
       plot,
       poly,  nroots, real_roots
export members, doc, _sbtr


include("types.jl")
include("utils.jl")
include("mathops.jl")
include("math.jl")
include("core.jl")
include("simplify.jl")
include("series.jl")
include("integrate.jl")
include("assumptions.jl")
include("poly.jl")
include("matrix.jl")
include("plot.jl")


## create some methods

for meth in union(core_sympy_methods,
                  simplify_sympy_meths,
                  series_sympy_meths,
                  integrals_sympy_methods,
                  summations_sympy_methods,
                  logic_sympy_methods,
                  polynomial_sympy_methods)

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
