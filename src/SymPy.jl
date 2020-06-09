"""

`SymPy` package to interface with Python's [SymPy library](http://www.sympy.org) through `PyCall`.

The basic idea is that a new type -- `Sym` -- is made to hold symbolic
objects.  For this type, the basic functions from SymPy and appropriate functions
of `Julia` are overloaded for `Sym` objects so that the expressions
are treated symbolically and not evaluated immediately. Instances of
this type are created by the constructor `Sym`, the function `symbols` or the macro
`@vars`.

On loading, a priviledged set of the functions from the `sympy` module
are defined as generic functions with their first argument narrowed to
symbolic types. Others may be accessed by qualification, as in
`sympy.trigsimp`. Calling `import_from(sympy)` will import the
rest. SymPy methods are called through Python's dot-call syntax.  To
find documentation on SymPy functions and methods, one should refer to
SymPy's [website](http://docs.sympy.org/latest/index.html).

Plotting is provided through  `Plots` recipes. For details, see
the help page for [`sympy_plotting`](@ref).

The package documentation provides many examples. 

"""
module SymPy


## minimal version of conveniently using SymPy in Julia
## uses getfield overloading to access sympy methods of Sym object

using PyCall

using SpecialFunctions
using LinearAlgebra


import Base: show
import Base: convert, promote_rule
import Base: getproperty
import Base: hash, ==
import Base: length, size
import Base.iterate
import Base: +, -, *, /, //, \, ^

export @vars, Sym, sympify, symbols, @symfuns, @syms
export SymMatrix, SymFunction
export PI, IM, oo, zoo, True, False
export N, subs

export sympy, sympy_core, sympy_matrices, import_from#, import_sympy
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
"""
    sympy

Variable from `pyimport("sympy")`. Numerous methods are available through Python's dot-call syntax. 
"""
const sympy  = PyCall.PyNULL()
"""
    sympy_core

Variable from `pyimport("sympy.core")`.
"""
const sympy_core =  PyCall.PyNULL()
"""
    sympy_matrices

Variable from `pyimport("sympy.matrices")`.
"""
const sympy_matrices =  PyCall.PyNULL()
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
    copy!(sympy_core, PyCall.pyimport("sympy.core"))
    copy!(sympy_matrices, PyCall.pyimport("sympy.matrices"))    
    copy!(PI.__pyobject__,  sympy.pi)
    copy!(IM.__pyobject__, sympy.I)
    copy!(oo.__pyobject__, sympy.oo)
    copy!(zoo.__pyobject__, sympy.zoo)
    copy!(True.__pyobject__, PyCall.PyObject(true))
    copy!(False.__pyobject__, PyCall.PyObject(false))


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
    pytype_mapping(sympy_matrices.ImmutableMatrix, SymMatrix)
    pytype_mapping(sympy_matrices.ImmutableDenseMatrix, SymMatrix)

    basictype = sympy_core.basic.Basic
    pytype_mapping(basictype, Sym)

    pytype_mapping(sympy_matrices.Matrix, Array{Sym})
    pytype_mapping(sympy_matrices.MatrixBase, Array{Sym})

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



    ## We *would* like to import some methods from the `sympy` module, but we can't as calling
    ## @eval to import from a different package causes precompilation issues.
    ## Rather, we only call `import_sympy` to print a list of commands to add to this file (as
    ## are added below). This is done by uncommenting the `println` statements in `import_from`.
    #import_sympy()

end


## On load, generic functions in the base modules that match a function in sympy.* are
## defined on `Sym` objects, as are those listed here:
##
priviledged = (:And, :Or, :Not, :Xor,
               #
               :apart, :cancel, :cse, :expand, :factor, :flatten, :nsimplify,
               :isolate, :simplify, :together, :unflatten,
               #
               :srepr,:doit,
               #
               :integrate, :line_integrate, :interpolate, :limit, :series, :summation,
               :hessian,
               #
               :prime, :multiplicity, :degree, :coeffs,
               #
               :DiracDelta, :Heaviside,
               #
               :linsolve, :nonlinsolve, :nroots, :nsolve, :pdsolve, :real_root,
               :real_roots, :root, :rootof, :roots, :rsolve, :solve, :solveset,
               :ode_order,
               #
               :Min, :Max, :Abs,:numer, :denom, :conjugate, :ln,
               #
               :intersection, :intervals, :isprime

               )


function in_base(uv)
    u = Symbol(uv[1])
    for M in base_Ms
        isdefined(M, u) && return true
    end
    false
end
is_function(uv) = SymPy.pycall_hasproperty(uv[2], :__class__) &&  occursin("unction", uv[2].__class__.__name__)



"""
    import_sympy

This method imports all functions from `mpmath` and a priviledged set
of functions from `sympy`, as well as the relational operators.

These functions are narrowed on their first argument being of type `SymbolicObject`.

A few modules are checked for namespace collisions.

If a function naturally takes an non-Symbolic argument as a first argument, then it can be qualified: e.g.
`sympy.sin(2)` (as opposed to `sin(Sym(2))`).

If a constructor is needed (which is not a function) then it must be
qualified. (E.g. `sympy.Function("F")`, though for this particular
case, there is `SymFunction` defined for convenience.)

"""
function import_sympy()
    if mpmath != PyCall.PyNULL()
        mps = _get_member_functions(mpmath, base_exclude)
        sps = Introspection.getmembers(sympy)
        imp = Tuple(Symbol.(setdiff(keys(mps), keys(sps))))
        import_from(mpmath, imp)
    end
    ## import from
    ## import_from(sympy)
    d = Introspection.getmembers(sympy)
    d1 = filter(uv -> in_base(uv) && is_function(uv), d)
    import_from(sympy, setdiff(Symbol.(collect(keys(d1))),  Symbol.(base_exclude)))
    import_from(sympy, priviledged)
    import_from(sympy, (:Ne,  :Le, :Eq, :Ge, :Gt,
                        :GreaterThan, :LessThan,
                        :StrictGreaterThan, :StrictLessThan,
                        :Equality, :Unequality
                        ), typ=:Number)
end

## :Lt is in Base.Order
import Base.Order: Lt
Lt(x::Number, args...;kwargs...) = sympy.Lt(x, args...; kwargs...)
export(Lt)


### Add generic methods and new methods
include("importexport.jl")

end # module
