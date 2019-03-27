module SymPy


## minimal version of conveniently using SymPy in Julia
## uses getfield overloading to access sympy methods of Sym object

using PyCall

using SpecialFunctions
using LinearAlgebra
using OffsetArrays


import Base: show
import Base: convert, promote_rule
import Base: getproperty
import Base: hash, ==
import Base: length, size
import Base.iterate
import Base: +, -, *, /, //, \, ^

export @vars, Sym, sympify, SymMatrix, SymFunction, @symfuns
export PI, IM, oo, zoo, True, False
export N

export sympy, import_from#, import_sympy
export free_symbols

include("types.jl")
include("constructors.jl")
include("utils.jl")
include("numbers.jl")
include("mathops.jl")
include("mathfuns.jl")
include("generic.jl")
include("matrix.jl")
include("sets.jl")
include("symfunction.jl")
include("assumptions.jl")
include("lambdify.jl")
include("patternmatch.jl")
include("permutations.jl")
include("plot_recipes.jl")

##################################################

pynull() = PyCall.PyNULL()
const sympy  = PyCall.PyNULL()
const mpmath = PyCall.PyNULL()
const combinatorics  = PyCall.PyNULL()

# core.sympy.numbers.*
"PI is symbolic `pi`"
global PI = Sym(pynull())
"IM is a symbolic `im`"
global IM = Sym(pynull())
"oo is a symbolic infinity. Example: `integrate(exp(-x), x, 0, oo)`."
global oo = Sym(pynull())
"zoo complex inifinity"
global zoo = Sym(pynull())
"True from SymPy"
global True = Sym(pynull())
"False from SymPy"
global False = Sym(pynull())



# Can not actually initiate many things until `sympy` is defined, so not until runtime
function __init__()

    ## Define sympy, mpmath, ...
    copy!(sympy, PyCall.pyimport_conda("sympy", "sympy"))
    copy!(PI.x,  sympy.pi)
    copy!(IM.x, sympy.I)
    copy!(oo.x, sympy.oo)
    copy!(zoo.x, sympy.zoo)
    copy!(True.x, PyCall.PyObject(true))
    copy!(False.x, PyCall.PyObject(false))


    # mpmath
    try
        PyCall.mpmath_init()
        copy!(mpmath, PyCall.pyimport_conda("mpmath", "mpmath"))
    catch err
       # can't load
    end

    # pull in alibrary
    copy!(combinatorics, PyCall.pyimport_conda("sympy.combinatorics", "sympy"))


    ## mappings from PyObjects to types.
    ## order here is fussy, as we needed ImmutableMatrix before Basic
    pytype_mapping(sympy.ImmutableMatrix, SymMatrix)
    pytype_mapping(sympy.ImmutableDenseMatrix, SymMatrix)

    basictype = sympy.basic.Basic
    pytype_mapping(basictype, Sym)

    pytype_mapping(sympy.Matrix, Array{Sym})
    pytype_mapping(sympy.matrices.MatrixBase, Array{Sym})

    pytype_mapping(sympy.FiniteSet, Set)

    pytype_mapping(sympy.combinatorics.permutations.Permutation, SymPermutation)
    pytype_mapping(combinatorics.perm_groups.PermutationGroup, SymPermutationGroup)

    if mpmath != PyCall.PyNULL()
        ## ignore warnings for now...
        mpftype = mpmath."mpf"
        pytype_mapping(mpftype, BigFloat) ## Raises warning!!!
        mpctype = mpmath."mpc"
        pytype_mapping(mpctype, Complex{BigFloat})
    end



    ## is this a good idea?
    ## could leave this out
    import_sympy()

end

"""
    import_sympy

This method imports all functions from both `mpmath` and `sympy` as well as the relational operators, which
don't get swept up by `import_from`.

These functions are specialized on their first argument being of type `SymbolicObject`.

It checks a few modules for namespace collisions.

If a function naturally takes an non-Symbolic argument as a first argument, then it can be qualified: e.g.
`sympy.sin(2)` (as opposed to `sin(Sym(2))`).

If a constructor is needed (which is not a function) then it must be
qualified. (E.g. `sympy.Function("F")`, though for this particular
case, there is `SymFunction` defined for convenience.)

"""
function import_sympy()
    if mpmath != PyCall.PyNULL()
        import_from(mpmath)
    end
    import_from(sympy)
    import_from(sympy, (:Ne, :Lt, :Le, :Eq, :Ge, :Gt,
                        :GreaterThan, :LessThan,
                        :StrictGreaterThan, :StrictLessThan,
                        :Equality, :Unequality
                        ), typ=:Number) ## Lt fails to come in?

end
## oddity???
Lt(x::Number, args...;kwargs...) = sympy.Lt(x, args...; kwargs...)
export(Lt)

end # module
