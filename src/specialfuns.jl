module SpecialFuncs

using PyCall
using SymPy

if VERSION < v"0.6.0-dev"
    for meth in [:gamma, :polygamma, :beta,
                 :airyai, :airybi,
                 :besseli, :besselj, :besselk, :bessely]
        eval(Expr(:import, :Base, meth))
    end
else
    ## how to handlethis deprecation phase without SpecialFuncs.jl, as that
    ## doesn't have v0.4 support?
    ## Here we need to qualify usage, as in `SpecialFuns.airyai`.
    for meth in  (:gamma, :polygamma, :beta,
               :airyai, :airybi,
               :besseli, :besselj, :besselk, :bessely)
        eval(Expr(:export, meth))
    end
end

for meth in (
             :jacobi,
             :gegenbauer,
             :chebyshevt, :chebyshevu,
             :legendre, :assoc_legendre,
             :hermite,
             :laguerre, :assoc_laguerre,
             :Ynm, :Ynm_c,
             :hankel1, :hankel2,
             :jn, :yn,
             :elliptic_e, :elliptic_k,
             :elliptic_f, :elliptic_pi
             )
    meth_name = string(meth)
    @eval begin
        @doc """
            `$($meth_name)`: a SymPy function.
                The SymPy documentation can be found through: http://docs.sympy.org/latest/search.html?q=$($meth_name)
                """ ->
        ($meth)(xs...;kwargs...) = sympy_meth($meth_name, xs...; kwargs...)
    end
    eval(Expr(:export, meth))
end


## these have (parameter, x) signature. Use symbolic x to call sympy version, othewise
## should dispatch to julia version.
for fn in (:besselj, :bessely, :besseli, :besselk)
    meth = string(fn)
#    eval(Expr(:import, :Base, fn))
    @eval ($fn)(nu::Number, x::Sym; kwargs...) = sympy_meth($meth, nu, x; kwargs...)
    @eval ($fn)(nu::Number, a::AbstractArray{Sym}) = map(x ->$fn(nu, x), a)
end


for meth in (:fresnels, :fresnelc, :Ei, :Si, :Ci,
             :airyai, :airybi
           )
    meth_name = string(meth)

    @eval begin
        @doc """<
`$($meth_name)`: a SymPy function.
The SymPy documentation can be found through: http://docs.sympy.org/latest/search.html?q=$($meth_name)
""" ->
        ($meth)(x::Sym;kwargs...) = sympy_meth($meth_name, x; kwargs...)
    end
#    @eval ($meth)(a::AbstractArray{Sym}) = map($meth, a)
    eval(Expr(:export, meth))
end


## :gamma, :beta... # need import
beta(x::Sym, y::Sym) = sympy_meth(:beta, x, y)
gamma(x::Sym) = sympy_meth(:gamma, x)
polygamma(nu::Sym, x::Sym) = sympy_meth(:polygamma, nu, x)


## Hyper and friends don't really have symbolic use...
"""

Evaluates the generalized hypergeometric function:
[hyper](http://docs.sympy.org/dev/modules/mpmath/functions/hypergeometric.html#hyper)

"""
hyper{T<:Number, S<:Number}(as::Vector{T}, bs::Vector{S}, z::Number) = sympy_meth(:hyper, as, bs, z)
export hyper

"""

[docs](http://docs.sympy.org/dev/modules/mpmath/functions/hypergeometric.html#meijer-g-function)

"""
function meijerg{T<:Number, S<:Number}(a1s::Vector{T}, a2s::Vector{T}, b1s::Vector{S}, b2s::Vector{S}, z::Number, r=1;kwargs...)
    sympy_meth(:meijerg, a1s, a2s, b1s, b2s, z;  kwargs...)
end
export meijerg

end
