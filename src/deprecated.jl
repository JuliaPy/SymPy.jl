# in SymPy, not SymPyCall

#Base.@deprecate_binding sympy_core sympy.core
#Base.@deprecate_binding sympy_matrices sympy.matrices


Base.@deprecate conjugate(x::Sym, args...; kwargs...)  conj(x, args...; kwargs...)  true

Base.@deprecate cse(ex::SymbolicObject, args...; kwargs...) sympy.cse(ex, args...; kwargs...) true
Base.@deprecate denom(ex::SymbolicObject, args...; kwargs...) sympy.denom(ex, args...; kwargs...) true
Base.@deprecate flatten(ex::SymbolicObject, args...; kwargs...) sympy.flatten(ex, args...; kwargs...) true
Base.@deprecate unflatten(ex::SymbolicObject, args...; kwargs...) sympy.unflatten(ex, args...; kwargs...) true
Base.@deprecate interpolate(ex::SymbolicObject, args...; kwargs...) sympy.interpolate(ex, args...; kwargs...) true
Base.@deprecate intervals(ex::SymbolicObject, args...; kwargs...) sympy.intervals(ex, args...; kwargs...) true
Base.@deprecate isolate(ex::SymbolicObject, args...; kwargs...) sympy.isolate(ex, args...; kwargs...) true
Base.@deprecate isprime(ex::SymbolicObject, args...; kwargs...) sympy.isprime(ex, args...; kwargs...) true
Base.@deprecate line_integrate(ex::SymbolicObject, args...; kwargs...) sympy.line_integrate(ex, args...; kwargs...) true
Base.@deprecate ln(ex::SymbolicObject, args...; kwargs...) sympy.ln(ex, args...; kwargs...) true
Base.@deprecate prime(ex::SymbolicObject, args...; kwargs...) sympy.prime(ex, args...; kwargs...) true
Base.@deprecate real_root(ex::SymbolicObject, args...; kwargs...) sympy.real_root(ex, args...; kwargs...) true
Base.@deprecate root(ex::SymbolicObject, args...; kwargs...) sympy.root(ex, args...; kwargs...) true
Base.@deprecate rootof(ex::SymbolicObject, args...; kwargs...) sympy.rootof(ex, args...; kwargs...) true
Base.@deprecate rsolve(ex::SymbolicObject, args...; kwargs...) sympy.rsolve(ex, args...; kwargs...) true
Base.@deprecate srepr(ex::SymbolicObject, args...; kwargs...) sympy.srepr(ex, args...; kwargs...) true
Base.@deprecate multiplicity(ex::SymbolicObject, args...; kwargs...) sympy.multiplicity(ex, args...; kwargs...) true
Base.@deprecate nsimplify(ex::SymbolicObject, args...; kwargs...) sympy.nsimplify(ex, args...; kwargs...) true
Base.@deprecate numer(ex::SymbolicObject, args...; kwargs...) sympy.numer(ex, args...; kwargs...) true
Base.@deprecate ode_order(ex::SymbolicObject, args...; kwargs...) sympy.ode_order(ex, args...; kwargs...) true
Base.@deprecate pdsolve(ex::SymbolicObject, args...; kwargs...) sympy.pdsolve(ex, args...; kwargs...) true
Base.@deprecate Abs(ex::SymbolicObject, args...; kwargs...) sympy.Abs(ex, args...; kwargs...) true
Base.@deprecate And(ex::SymbolicObject, args...; kwargs...) sympy.And(ex, args...; kwargs...) true
Base.@deprecate DiracDelta(ex::SymbolicObject, args...; kwargs...) sympy.DiracDelta(ex, args...; kwargs...) true
Base.@deprecate Equality(ex::SymbolicObject, args...; kwargs...) sympy.Equality(ex, args...; kwargs...) true
Base.@deprecate GreaterThan(ex::SymbolicObject, args...; kwargs...) sympy.GreaterThan(ex, args...; kwargs...) true
Base.@deprecate LessThan(ex::SymbolicObject, args...; kwargs...) sympy.LessThan(ex, args...; kwargs...) true
Base.@deprecate Max(ex::SymbolicObject, args...; kwargs...) sympy.Max(ex, args...; kwargs...) true
Base.@deprecate Min(ex::SymbolicObject, args...; kwargs...) sympy.Min(ex, args...; kwargs...) true
Base.@deprecate Not(ex::SymbolicObject, args...; kwargs...) sympy.Not(ex, args...; kwargs...) true
Base.@deprecate Or(ex::SymbolicObject, args...; kwargs...) sympy.Or(ex, args...; kwargs...) true
Base.@deprecate StrictGreaterThan(ex::SymbolicObject, args...; kwargs...) sympy.StrictGreaterThan(ex, args...; kwargs...) true
Base.@deprecate StrictLessThan(ex::SymbolicObject, args...; kwargs...) sympy.StrictLessThan(ex, args...; kwargs...) true
Base.@deprecate Unequality(ex::SymbolicObject, args...; kwargs...) sympy.Unequality(ex, args...; kwargs...) true
Base.@deprecate Xor(ex::SymbolicObject, args...; kwargs...) sympy.Xor(ex, args...; kwargs...) true

Base.@deprecate plot_parametric_surface(exs, args...; kwargs...) sympy.plotting.plot3d_parametric_surface(exs..., args...; kwargs...) true
Base.@deprecate plot_implicit(ex, args...; kwargs...) sympy.plotting.plot_implicit(ex, args...; kwargs...) true


# mpmath @deprecate
function expj(ex::SymbolicObject, args...; kwargs...)
    Base.depwarn("The expj function is deprecated. To use this feature, you would need to import the python library `mpmath` and then call as `mpmath.expj(...)`.", :expj)
    mpmath.expj(ex, args...; kwargs...)
end
export expj

function expjpi(ex::SymbolicObject, args...; kwargs...)
    Base.depwarn("The expjpi function is deprecated. To use this feature, you would need to import the python library `mpmath` and then call as `mpmath.expjpi(...)`.", :expjpi)
    mpmath.expjpi(ex, args...; kwargs...)
end
export expjpi

function fac(ex::SymbolicObject, args...; kwargs...)
    Base.depwarn("The fac function is deprecated. To use this feature, you would need to import the python library `mpmath` and then call as `mpmath.fac(...)`.", :fac)
    mpmath.fac(ex, args...; kwargs...)
end
export fac

function nint(ex::SymbolicObject, args...; kwargs...)
    Base.depwarn("The nint function is deprecated. To use this feature, you would need to import the python library `mpmath` and then call as `mpmath.nint(...)`.", :nint)
    mpmath.nint(ex, args...; kwargs...)
end
export nint

function fib(ex::SymbolicObject, args...; kwargs...)
    Base.depwarn("The fib function is deprecated. To use this feature, you would need to import the python library `mpmath` and then call as `mpmath.fib(...)`.", :fib)
    mpmath.fib(ex, args...; kwargs...)
end
export fib

function monitor(ex::SymbolicObject, args...; kwargs...)
    Base.depwarn("The monitor function is deprecated. To use this feature, you would need to import the python library `mpmath` and then call as `mpmath.monitor(...)`.", :monitor)
    mpmath.monitor(ex, args...; kwargs...)
end
export monitor

function bernfrac(ex::SymbolicObject, args...; kwargs...)
    Base.depwarn("The bernfrac function is deprecated. To use this feature, you would need to import the python library `mpmath` and then call as `mpmath.bernfrac(...)`.", :bernfrac)
    mpmath.bernfrac(ex, args...; kwargs...)
end
export bernfrac

function doctests(ex::SymbolicObject, args...; kwargs...)
    Base.depwarn("The doctests function is deprecated. To use this feature, you would need to import the python library `mpmath` and then call as `mpmath.doctests(...)`.", :doctests)
    mpmath.doctests(ex, args...; kwargs...)
end
export doctests

function ei(ex::SymbolicObject, args...; kwargs...)
    Base.depwarn("The ei function is deprecated. To use this feature, you would need to import the python library `mpmath` and then call as `mpmath.ei(...)`.", :ei)
    mpmath.ei(ex, args...; kwargs...)
end
export ei

function timing(ex::SymbolicObject, args...; kwargs...)
    Base.depwarn("The timing function is deprecated. To use this feature, you would need to import the python library `mpmath` and then call as `mpmath.timing(...)`.", :timing)
    mpmath.timing(ex, args...; kwargs...)
end
export timing

function rgamma(ex::SymbolicObject, args...; kwargs...)
    Base.depwarn("The rgamma function is deprecated. To use this feature, you would need to import the python library `mpmath` and then call as `mpmath.rgamma(...)`.", :rgamma)
    mpmath.rgamma(ex, args...; kwargs...)
end
export rgamma

function e1(ex::SymbolicObject, args...; kwargs...)
    Base.depwarn("The e1 function is deprecated. To use this feature, you would need to import the python library `mpmath` and then call as `mpmath.e1(...)`.", :e1)
    mpmath.e1(ex, args...; kwargs...)
end
export e1



# mull over
#XXX intersection -> intersect
#XXX cse
#XXX flatten, unflatten
#XXX interpolate
#XXX intervals
# (:isolate
# ,:isprime
# ,:line_integrate
# ,:ln
# ,:monitor
# ,:prime)

## Done
# # in mpmath
# #XXX expj
# (:expjpi
# ,:fac
# ,:nint
# ,:fib
# ,:monitor
# ,:bernfrac
# ,:doctests
# ,:ei
# ,:timing
# ,:rgamma
# ,:expjpi
# ,:e1)

# add depwarns
# SymMatrix
# Base.@deprecate_binding Removed
#Base.@deprecate_binding SymPermutation nothing
#Base.@deprecate_binding SymPermutationGroup nothing
# transpositions
# sympy_plotting
# plot_implicit
# plot_parametric_surface
# import_from
# VectorField
# elements
# :Lambda

# # # use sympy.XXX
# (:Abs
# ,:And
# ,:DiracDelta
# ,:Equality
# ,:GreaterThan
# ,:LessThan
# ,:Max
# ,:Min
# ,:Not
# ,:Or
# ,:StrictGreaterThan
# ,:StrictLessThan
# ,:Unequality
# ,:Xor)

# (:real_root
# ,:root
# ,:rootof
# ,:rsolve
# ,:srepr
# ,:multiplicity
# ,:nsimplify
# ,:numer
# ,:denom
# ,:ode_order
# ,:pdsolve)

# document
# @symfuns
# @vars
