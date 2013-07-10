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


end
