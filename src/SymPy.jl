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
import Base: factor, gcd, expand
import Base: !=, ==
import Base:  LinAlg.det, LinAlg.inv, LinAlg.conj, 
              cross, eigvals, eigvecs, rref, trace
import Base: promote_rule

export sympy, call_meth, call_meth_nosimplify, call_object_meth, call_matrix_meth
export Sym, @sym_str, @syms, symbols
export pprint, latex, jprint
export SymFunction, SymMatrix,
       n,  subs,
       simplify, nsimplify, factor, collect, separate, 
       radsimp, ratsimp, trigsimp, powsimp, combsimp,hypersimp,
       fraction,
       primitive, gcd, resultant, cancel,
       expand, together, apart,
       limit, diff, 
       series, integrate, 
       summation,
       I, oo,
       Ylm, assoc_legendre, chebyshevt, legendre, hermite,
       dsolve,
       plot,
       poly, nroots, real_roots
export jacobian, hessian
export members, doc, _str


include("utils.jl")
include("math.jl")
include("plot.jl")

end
