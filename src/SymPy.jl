module SymPy


## minimal version of conveniently using SymPy in Julia
## uses getfield overloading to access sympy methods of Sym object

using PyCall

using SpecialFunctions
using LinearAlgebra
using Random
using Statistics



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

export sympy, import_from
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


# ## string
# convert(::Type{Sym}, o::AbstractString) = sympy_meth(:sympify, o)
# convert(::Type{Sym}, o::Symbol) = sympy_meth(:sympify, string(o))

# ## function
# convert(::Type{Function}, ex::Sym) = lambdify(ex)

# ## we usually promote to Sym objects, but here we want to promote to functions
# ## so [x, sin] -> will be plottable as two functions
# promote_rule(::Type{T}, ::Type{S}) where {T<:SymbolicObject, S<:Function} = S

##################################################




# ## length of object
# function length(x::SymbolicObject)
#     haskey(PyObject(x), :length) && return PyObject(x)[:length]
#     sz = size(x)
#     length(sz) == 0 && return(0)
#     *(sz...)
# end

# ## size
# function size(x::SymbolicObject)
#     return ()
# end

# function size(x::SymbolicObject, dim::Integer)
#     if dim <= 0
#         error("dimension out of range")

#     else
#         return 1
#     end
# end


# ## Iterator for Sym
# iterate(x::Sym) = (x.x, 0)
# iterate(x::Sym, state) = nothing
##################################################

pynull() = PyCall.PyNULL()
const sympy  = PyCall.PyNULL()
const mpmath = PyCall.PyNULL()

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


## Need a structure like this
#import_fn=((:sin, Base), ...)
#rename_sympy_fn = ((:Abs, :abs, Base), ...)
#extend_fn = (:Abs2, :(abs(x)^2))






# Can actually initiate many things until `sympy` is defined, so not until runtime
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


    ## mappings from PyObjects to types.
    basictype = sympy.basic.Basic
    pytype_mapping(basictype, Sym)
    pytype_mapping(sympy.Matrix, SymMatrix)
    pytype_mapping(sympy.FiniteSet, Set)

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

## Import all from sympy, mpmath
function import_sympy()
    if mpmath != PyCall.PyNULL()
        import_from(mpmath)
    end
    import_from(sympy)
    import_from(sympy, (:Ne, :Lt, :Le, :Eq, :Ge, :Gt)) ## Lt fails to come in?

end
Lt(x::SymbolicObject, args...;kwargs...) = sympy.Lt(x, args...; kwargs...)
export(Lt)

end # module
