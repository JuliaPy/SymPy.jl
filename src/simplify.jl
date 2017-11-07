## port methods of simplify module of SymPy
## from http://docs.sympy.org/0.7.2/modules/simplify/simplify.html

## simple methods (x, args) -> y (y coercion happens via PyCall)
simplify_sympy_meths = (:collect, :rcollect, :separate,
                        :separatevars,
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
`$(cse)`: Perform common subexpression elimination on an expression.

[cf.](http://docs.sympy.org/latest/modules/simplify/simplify.html)

Example: (from man page)
```
@vars x w y z x0 x1
cse(((w + x + y + z)*(w + y + z))/(w + x)^3), ([(x0, y + z), (x1, w + x)], [(w + x0)*(x0 + x1)/x1^3]) # tuple of replacements and reduced expressions.

""" ->
function cse{T<:SymbolicObject}(ex::Union{T, AbstractArray{T}}, args...; kwargs...)
    a, b = sympy_meth(:cse, ex, args...; kwargs...)
    a, oftype(ex, b[1])
end
export(cse)

## didn't do traversal tools, EPath tools
