## port methods of simplify module of SymPy
## from http://docs.sympy.org/0.7.2/modules/simplify/simplify.html

## simple methods (x, args) -> y (y coercion happens via PyCall)
simplify_sympy_meths = (:collect, :rcollect, :separatevars,
                        :radsimp, :ratsimp, :trigsimp, :besselsimp,
                        :powsimp, :combsimp, :hypersimp,
                        :fraction,
                        :simplify, :nsimplify, :cse,
                        :posify, :powdenest, :sqrtdenest,
                        :logcombine, :hyperexpand)



## didn't do traversal tools, EPath tools