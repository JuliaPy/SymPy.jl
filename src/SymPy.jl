module SymPy

using GoogleCharts
using PyCall

@pyimport sympy

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
import Base: factorial, gamma, beta
import Base: solve, length,  size
import Base: !=, ==
import Base:  LinAlg.det, LinAlg.inv, LinAlg.conj, 
              cross, eigvals, eigvecs, rref, trace
import Base: promote_rule

export sympy
export Sym, @sym_str, @syms
export pprint, latex
export SymFunction, SymMatrix,
       n, subs,
       expand, together, apart,
       limit, diff, series, integrate, summation,
       I, oo,
       Ylm, assoc_legendre, chebyshevt, legendre, hermite,
       dsolve,
       plot,
       poly, nroots, real_roots
export members, doc, _str


include("utils.jl")
include("math.jl")
include("plot.jl")

end
