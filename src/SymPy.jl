module SymPy

using GoogleCharts
using PyCall

@pyimport sympy

import Base.getindex
import Base.show
import Base.convert, Base.complex
import Base.sin, Base.cos, Base.tan, Base.sinh, Base.cosh, Base.tanh, Base.asin, Base.acos,
       Base.atan, Base.asinh, Base.acosh, Base.atanh, Base.sec, Base.csc, Base.cot, Base.asec,
       Base.acsc, Base.acot, Base.sech, Base.csch, Base.coth, Base.asech, Base.acsch, Base.acoth,
       Base.sinc, Base.cosc, Base.cosd, Base.cotd, Base.cscd, Base.secd, Base.sind, Base.tand,
       Base.acosd, Base.acotd, Base.acscd, Base.asecd, Base.asind, Base.atand, Base.atan2,
       Base.radians2degrees, Base.degrees2radians, Base.log, Base.log2,
       Base.log10, Base.log1p, Base.exponent, Base.exp, Base.exp2, Base.expm1, Base.cbrt, Base.sqrt,
       Base.square, Base.erf, Base.erfc, Base.erfcx, Base.erfi, Base.dawson, Base.ceil, Base.floor,
       Base.trunc, Base.round, Base.significand
       Base.abs
import Base.factorial, Base.gamma, Base.beta
import Base.solve, Base.length
import Base.==

export sympy
export Sym, @sym_str
export pprint, latex
export SymFunction, SymMatrix,
       n, subs,
       expand, together, apart,
       abs,
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
