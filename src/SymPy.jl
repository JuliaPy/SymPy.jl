module SymPy

using GoogleCharts
using PyCall

## Hack to workaround issue with reserved names and types on initialization
function our_pywrap(o::PyObject)
#    @pyinitialize
    members = convert(Vector{(String,PyObject)}, 
                      pycall(PyCall.inspect["getmembers"], PyObject, o))
    filter!(m -> !contains(PyCall.reserved, m[1]), members)
    tname = gensym("PyCall_PyWrapper")
    @eval begin
        $(Expr(:type, true, Expr(:<:, tname, :PyWrapper),
               Expr(:block, :(___jl_PyCall_PyObject___::PyObject),
                    map(m -> Expr(:(::), symbol(m[1] * "__" ), 
                                  PyCall.typesymbol(pytype_query(m[2]))), 
                        members)...)))
        $(Expr(:call, tname, o,
               [ convert(PyAny, members[i][2]) for i = 1:length(members) ]...))
    end
end
sympy = our_pywrap(pyimport("sympy"))

## @pyimport sympy


import Base.getindex
import Base.show
import Base.convert
import Base.sin, Base.cos, Base.tan, Base.sinh, Base.cosh, Base.tanh, Base.asin, Base.acos,
       Base.atan, Base.asinh, Base.acosh, Base.atanh, Base.sec, Base.csc, Base.cot, Base.asec,
       Base.acsc, Base.acot, Base.sech, Base.csch, Base.coth, Base.asech, Base.acsch, Base.acoth,
       Base.sinc, Base.cosc, Base.cosd, Base.cotd, Base.cscd, Base.secd, Base.sind, Base.tand,
       Base.acosd, Base.acotd, Base.acscd, Base.asecd, Base.asind, Base.atand, Base.atan2,
       Base.radians2degrees, Base.degrees2radians, Base.log, Base.log2,
       Base.log10, Base.log1p, Base.exponent, Base.exp, Base.exp2, Base.expm1, Base.cbrt, Base.sqrt,
       Base.square, Base.erf, Base.erfc, Base.erfcx, Base.erfi, Base.dawson, Base.ceil, Base.floor,
       Base.trunc, Base.round, Base.significand
import Base.factorial, Base.gamma, Base.beta
import Base.solve

export sympy
export Sym, @sym_str
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
export members

       

include("utils.jl")
include("math.jl")
include("plot.jl")

end
