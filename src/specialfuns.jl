
## Hyper and friends don't really have symbolic use...
"""

Evaluates the generalized hypergeometric function:
[hyper](http://docs.sympy.org/dev/modules/mpmath/functions/hypergeometric.html#hyper)

"""
hyper{T<:Number, S<:Number}(as::Vector{T}, bs::Vector{S}, z::Number) = sympy_meth(:hyper, map(project,as), map(project,bs), z)
export hyper

"""

[docs](http://docs.sympy.org/dev/modules/mpmath/functions/hypergeometric.html#meijer-g-function)

"""
function meijerg{T<:Number, S<:Number}(a1s::Vector{T}, a2s::Vector{T}, b1s::Vector{S}, b2s::Vector{S}, z::Number, r=1;kwargs...)
    as = [map(project,a1s), map(project,a2s)]
    bs = [map(project,b1s), map(project,b2s)]
    sympy_meth(:meijerg, as, bs, project(z), r;  [(k,project(v)) for (k,v) in kwargs]...)
end
export meijerg

