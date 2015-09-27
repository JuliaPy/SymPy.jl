VERSION >= v"0.4.0-" && __precompile__(true) 

## TODO:
## * make tests work under travis
## * tidy up code
## * wait out precompile

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

using PyCall, Conda
using Requires 

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
       sinpi, cospi,
       log, log2,
       log10, log1p, exponent, exp, exp2, expm1, cbrt, sqrt,
       erf, erfc, erfcx, erfi, erfcinv, dawson, ceil, floor,
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
import Base: +, -, *, /, \
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
    import Base: radians2degrees, degrees2radians
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
       rhs, lhs, args,
       jacobian, hessian,
       Max, Min
export PI, E, IM, oo
export relation, piecewise
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
include("subs.jl")
include("simplify.jl")
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


## For precompilation we must put PyCall instances in __init__:
function __init__()
    
    ## Define sympy, mpmath, ...
    try
        copy!(sympy, pyimport("sympy"))
    catch e
        error("Failed to pyimport(\"sympy\"): SymPy will not work until you have a functioning sympy module.  ", e)
        if PyCall.conda
            info("Installing sympy via the Conda package...")
            Conda.add("sympy")
            copy!(sympy, pyimport("sympy"))
        else
            error("""Failed to pyimport("sympy"): SymPy will not work until you have a functioning sympy module.

                  For automated SymPy installation, try configuring PyCall to use the Conda Python distribution within Julia.  Relaunch Julia and run:
                        ENV["PYTHON"]=""
                        Pkg.build("PyCall")
                        using SymPy

                  pyimport exception was: """, e)
        end
    end

    ## mappings from PyObjects to types.
    basictype = sympy[:basic]["Basic"]
    pytype_mapping(basictype, Sym)

    polytype = sympy[:polys]["polytools"]["Poly"]
    pytype_mapping(polytype, Sym)

    try
        matrixtype = sympy[:matrices]["MatrixBase"]
        pytype_mapping(matrixtype, SymMatrix)
    catch e
    end


    ## Makes it possible to call in a sympy method, witout worrying about Sym objects

    global call_sympy_fun(fn::Function, args...; kwargs...) = fn(map(project, args)...; [(k,project(v)) for (k,v) in kwargs]...)
    global call_sympy_fun(fn::PyCall.PyObject, args...; kwargs...) = call_sympy_fun(convert(Function, fn), args...; kwargs...)

    ## Main interface to methods in sympy
    ## sympy_meth(:name, ars, kwars...)
    global sympy_meth(meth::Symbol, args...; kwargs...) = begin
        ans = call_sympy_fun(convert(Function, sympy[meth]), args...; kwargs...)
        ## make nicer...
        if isa(ans, Vector)
            ans = Sym[i for i in ans]
        end
        ans
    end
    global object_meth(object::SymbolicObject, meth::Symbol, args...; kwargs...)  =  begin
        call_sympy_fun(project(object)[meth],  args...; kwargs...)
    end
    global call_matrix_meth(object::SymbolicObject, meth::Symbol, args...; kwargs...) = begin
        out = object_meth(object, meth, args...; kwargs...)
        if isa(out, SymMatrix) 
            convert(Array{Sym}, out)
        elseif  length(out) == 1
            out 
        else
            map(u -> isa(u, SymMatrix) ? convert(Array{Sym}, u) : u, out)
        end
    end

    ##
    init_logical()
    init_math()
    init_mpmath()
    init_plot()
end

end
