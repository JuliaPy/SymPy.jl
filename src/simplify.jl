## port methods of simplify module of SymPy
## from http://docs.sympy.org/0.7.2/modules/simplify/simplify.html

## simple methods (x, args) -> y (y coercion happens via PyCall)
simplify_sympy_meths = (:collect, :rcollect, :separatevars,
                        :radsimp, :ratsimp, :trigsimp, :besselsimp,
                        :powsimp, :combsimp, :hypersimp,
                        :fraction,
                        :simplify, :nsimplify,
                        :posify, :powdenest, :sqrtdenest,
                        :logcombine, :hyperexpand)

                            
expand_sympy_meths = (:expand_trig,
                      :expand_power_base, :expand_power_exp,
                      :expand_log,
                      :expand_func,
                      :hyperexpand
                      )

## special case cse due to output
## If this pattern is common, will need to collect and use metaprogramming.
@doc """
`$(cse)`: a SymPy function.
The SymPy documentation can be found through: http://docs.sympy.org/latest/search.html?q=cse
""" ->
cse{T<:SymbolicObject}(ex::T, args...; kwargs...) = sympy_meth(:cse, ex, args...; kwargs...)
cse{T<:SymbolicObject}(ex::Vector{T}, args...; kwargs...) = sympy_meth(:cse, ex, args...; kwargs...)
        
function cse{T<:SymbolicObject, N}(ex::Array{T, N}, args...; kwargs...)
    a,b = cse(ex[:], args...; kwargs...)
    bb = convert(Array{Sym},  reshape(b, size(ex)))
    a, bb
end
export(cse)

## didn't do traversal tools, EPath tools
