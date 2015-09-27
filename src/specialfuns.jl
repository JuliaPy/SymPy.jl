## simple (x::Union(Sym, Number;...) signature, export
typealias SymOrNumber @compat Union{Sym, Number}
for fn in (
           :hankel1, :hankel2,             # hankel function of second kind H_n^2(x) = J_n(x) - iY_n(x)
           :legendre,
           :jacobi, 
           :gegenbauer,
           :hermite,
           :laguerre
           )
    meth = string(fn)
    @eval ($fn)(xs::SymOrNumber...;kwargs...) = sympy_meth(symbol($meth), xs...; kwargs...)
    eval(Expr(:export, fn))
end



## these have (parameter, x) signature. Use symbolic x to call sympy version, othewise
## should dispatch to julia version.
for fn in (:besselj, :bessely, :besseli, :besselk)
    meth = string(fn)
    @eval ($fn)(nu::SymOrNumber, x::Sym;kwargs...) = sympy_meth(symbol($meth), x; kwargs...)
    @eval ($fn)(nu::SymOrNumber, a::Array{Sym}) = map(x ->$fn(nu, x), a)
end


## :gamma, :beta, # need import
beta(x::Sym, y::Sym) = sympy_meth(:beta, x, y)
gamma(x::Sym, y::Sym) = sympy_meth(:gamma, x, y)


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

