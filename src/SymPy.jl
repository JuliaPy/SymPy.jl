"""
`SymPy` package to interface with Python's [SymPy library](http://www.sympy.org) through `SymPyCore` and `PyCall`.
"""
module SymPy


using SymPyCore

using PyCall

const _PyType = PyCall.PyObject
_pynull() = PyCall.PyNULL()   # for more generic usage
_copy!(a, b) = PyCall.copy!(a,b)     # for more generic usage
_pyimport(a) = PyCall.pyimport(a)
_pyimport_conda(a,b) = PyCall.pyimport_conda(a, b)
_pyobject(x) = PyCall.PyObject(x)
_pytype_mapping(typ,a)  = PyCall.pytype_mapping(typ, a)

# for 1.6, we can't pass in _pybuiltin so we just
# paste in here sympy.jl and modify.
# not DRY!
#core_src_path = joinpath(pathof(SymPy.SymPyCore), "../../src/SymPy")
#include(joinpath(core_src_path, "sympy.jl"))
## Common code

using LinearAlgebra
import SpecialFunctions
import CommonEq
import CommonSolve

## XXX This should be in a file in SymPyCore...
#=
Several functions are exported

* generic functions in base `Julia` having a `sympy` counterpart have
  methods defined to dispatch on the first argument. (Basically,
  `Base.sin(x::Sym) = \uparrow(_sympy_.sin(\downarrow(x))`.) These are
  defined in the `generic_methods` list in `SymPyCore` and read in via
  the package once a Python bridge is in place.

* Some object methods, such as `ex.subs(...)` have exported functions
  that use the `Julian` order `subs(ex, ...)`. All object methods
  should be callable using the python syntax `obj.method(...)`.

* Functions in `sympy` that are foundational (e.g., `simplify`) have
  methods created dispatch on the first argument being symbolic
  (typically type `Sym`). These are exported. Other functions, e.g.,
  `trigsimp` may be called via `sympy.trigsimp(...)`.
=#

## import/exports
import SymPyCore: ↑, ↓, ↓ₖ
import SymPyCore: SymbolicObject, Sym, SymFunction
import SymPyCore: symbols, free_symbols
import SymPyCore: simplify, expand, together, apart, factor, cancel
import SymPyCore: solve, dsolve, nsolve, linsolve, nonlinsolve, solveset
import SymPyCore: subs, lambdify, simplify
import SymPyCore: ask, doit
import SymPyCore: N
import SymPyCore: limit, diff, integrate, Differential, Heaviside
import SymPyCore: rhs, lhs
import SymPyCore: Wild, Permutation, PermutationGroup
import SymPyCore: ∨, ∧, ¬  # infix logical operators

# more exports defined in SymPyCore/src/gen_methods_sympy
export Sym, SymFunction
export sympy, PI, E, IM, oo, zoo
export @syms, sympify, symbols, free_symbols
export simplify, expand, together, apart, factor, cancel
export solve, dsolve, nsolve, linsolve, nonlinsolve, solveset
export real_roots,  nroots # roots
export sign #,degree
export series, summation, hessian
export subs, lambdify
export ask, doit, rewrite
export N
export limit, diff, integrate, Differential, Heaviside
export rhs, lhs
export Wild #, cse
export Permutation, PermutationGroup
export ∨, ∧, ¬
export 𝑄, 𝑆, Introspection

export ↓, ↑

# emacs and indent; this goes last
import SymPyCore: Lt, ≪, Le, ≦, Eq, ⩵, Ne, ≶, ≷, Ge, ≫, Gt, ≧
export  Lt, ≪, Le, ≦, Eq, ⩵, Ne, ≶, ≷, Ge, ≫, Gt, ≧



# exported symbols and their python counterparts
const _sympy_  = _pynull()
const sympy = Sym(_sympy_)

const _combinatorics_ = _pynull()
const combinatorics = Sym(_combinatorics_)

const _PI_ =_pynull()
const PI = Sym(_PI_)

const _E_ =_pynull()
const E = Sym(_E_)

const _IM_ =_pynull()
const IM = Sym(_IM_)

const _oo_ =_pynull()
const oo = Sym(_oo_)

const _zoo_ =_pynull()
const zoo = Sym(_zoo_)

Base.@deprecate_binding TRUE Sym(true)
Base.@deprecate_binding FALSE Sym(false)

const _𝑄_ = _pynull()
const 𝑄 = Sym(_𝑄_)

const _𝑆_ = _pynull()
const 𝑆 = Sym(_𝑆_)

# ugly, but needed to speed up python_connection
const _pyset_ = _pynull()
const _pytuple_ = _pynull()
const _pylist_ = _pynull()
const _pydict_ = _pynull()
const _bool_ = _pynull()
const _types_  = _pynull()
const _ModuleType_ = _pynull()
const _FiniteSet_ = _pynull()
const _MutableDenseMatrix_ = _pynull()

function __init__()

    ## Define sympy, mpmath, ...
    _copy!(_sympy_, _pyimport_conda("sympy", "sympy"))
    _copy!(_combinatorics_, _pyimport_conda("sympy.combinatorics", "sympy"))

    _copy!(_PI_, _sympy_.pi)
    _copy!(_E_, _sympy_.E)
    _copy!(_IM_, _sympy_.I)
    _copy!(_oo_, _sympy_.oo)
    _copy!(_zoo_, _sympy_.zoo)
    _copy!(_𝑆_, _sympy_.S)
    _copy!(_𝑄_, _sympy_.Q)

    # ugly, avoids allocation in PyCall
    _pybuiltin = PyCall.builtin  # <-- needed with 1.6 + SymPy
    _copy!(_pyset_, getproperty(_pybuiltin, :set))
    _copy!(_pytuple_, getproperty(_pybuiltin, :tuple))
    _copy!(_pylist_, getproperty(_pybuiltin, :list))
    _copy!(_pydict_, getproperty(_pybuiltin, :dict))
    _copy!(_bool_, getproperty(_pybuiltin, :bool))
    _copy!(_types_, _pyimport_conda("types", "types"))
    _copy!(_ModuleType_, getproperty(_types_, :ModuleType))
    _copy!(_FiniteSet_, _sympy_.FiniteSet)
    _copy!(_MutableDenseMatrix_, _sympy_.MutableDenseMatrix)


end


# includes
core_src_path = joinpath(pathof(SymPyCore), "../../src/SymPy")
include(joinpath(core_src_path, "constructors_sympy.jl"))
include(joinpath(core_src_path, "gen_methods_sympy.jl"))
include(joinpath(core_src_path, "additional_methods_sympy.jl"))
include(joinpath(core_src_path, "show_sympy.jl"))


include("python_connection.jl")

end # module
