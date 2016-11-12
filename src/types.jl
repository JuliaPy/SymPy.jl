## Code for our Types and conversion methods


## Symbol class for controlling dispatch
abstract SymbolicObject <: Number

## Basic types defined here
"""

* `Sym("x")`
* `Sym(:x)`
* `Sym("x, y, z")`

The `Sym` type is an immutable type holding a reference to an
underlying python-based SymPy object. Many methods are extended to the
`Sym` type. Instances can be constructed in many ways. The one caveat
is the variables can not be function names in base.

"""
immutable Sym <: SymbolicObject
    x::PyCall.PyObject
end
Sym(s::SymbolicObject) = s

## Matrix type
immutable SymMatrix <: SymbolicObject
    x::PyCall.PyObject
end

## sets
immutable SymSet <: SymbolicObject
    x::PyCall.PyObject
end


## complex float
## this cause issue with printing on non-complex objects
#mpctype = sympy.mpmath["ctx_mp_python"]
#pytype_mapping(mpctype, Sym)

## some typealiases
typealias SymOrReal @compat Union{Sym, Real}
typealias SymOrNumber @compat Union{Sym, Number}
typealias SymOrString @compat Union{Sym, AbstractString}
typealias SymbolicTypes  @compat Union{AbstractString, Symbol, SymbolicObject}

PyCall.PyObject(x::SymbolicObject) = x.x

## Promotion 
## promote up to symbolic so that math ops work
promote_rule{T<:SymbolicObject, S<:Number}(::Type{T}, ::Type{S} ) = T



## Conversion
convert(::Type{Sym}, o::PyCall.PyObject) = Sym(o)
convert(::Type{PyObject}, s::Sym) = s.x


function convert(::Type{Tuple}, o::PyCall.PyObject)
    ## check that o is a tuple?
    ## PyCall.pytypeof(o)
    n = o[:__len__]()
    ntuple(i -> o[:__getitem__](i-1), n)
end

## rational
convert{T<:SymbolicObject}(::Type{T}, x::Rational) = sympy_meth(:Rational, x.num, x.den)::T

## big. Need mpmath installed separately -- not as a SymPy module as that is how it is called in PyCall
convert{T<:SymbolicObject}(::Type{T}, x::BigFloat) = Sym(PyCall.PyObject(x))::T
convert(::Type{Sym}, x::Complex{BigFloat}) = Sym(PyCall.PyObject(x))::Sym

## real
convert{S<:SymbolicObject, T <: Real}(::Type{S}, x::T) = sympy_meth(:sympify, x)::S
convert{T <: Real}(::Type{T}, x::Sym) = convert(T, project(x))


## complex
## IM is SymPy's "i" (sympy[:I], not Python's
## Sym(PyCall.PyObject(im)) which gives 1j.
function convert(::Type{Sym}, x::Complex)
    y = ifelse(isa(x, Complex{Bool}), real(x) + imag(x) * im, x)
    real(y) + imag(y) * IM
end
convert(::Type{Complex}, x::Sym) = complex(map(x -> convert(Float64, x), x[:as_real_imag]())...)::Sym
complex(x::Sym) = convert(Complex, x)
complex(xs::Array{Sym}) = map(complex, xs)

## matrices
convert(::Type{SymMatrix}, o::PyCall.PyObject) = SymMatrix(o)
convert(::Type{Sym}, o::SymMatrix) = Sym(o.x)
convert(::Type{SymMatrix}, o::Sym) = SymMatrix(o.x)

## string
convert(::Type{Sym}, o::AbstractString) = sympy_meth(:sympify, o)
convert(::Type{Sym}, o::Symbol) = sympy_meth(:sympify, string(o))


"""

Get the free symbols in a more convenient form than as returned by `free_symbols`.

Just `free_symbols` now does the same thing. This function will be deprecated.
"""    
function get_free_symbols(ex::Sym)
    free = free_symbols(ex)
    n = free[:__len__]()
    n == 0 && return(Sym[])

    vars = [free[:pop]()]
    if n > 1
        for i in 1:(n-1)
            push!(vars, free[:pop]())
        end
    end
    vars
end

## Conversion to function a bit hacky
## we use free_symbols to get the free symbols, then create a function
## with arguments in this order. No problem with only one variable, but
## may be confusing when more than one in ex.
## Output is symbolic. Conversion is necessary to use output as Julia values.
## SymPy has `lamdify` for this task too.
## function convert(::Type{Function}, ex::Sym)
##     vars = free_symbols(ex)
##     len = length(vars)
##     if len == 0
##         return x -> ex
##     end
##     local out
##     (args...) -> begin
##         out = ex
##         for i in 1:len
##             out = object_meth(out, :subs, vars[i], args[i]) #convert(Function, out[:subs])(project(vars[i]), args[i])
##         end
##         out
##     end
## end

## ## For plotting we need to know if a function has 1, 2, or 3 free variables
## function as_nfunction(ex::Sym, nvars=1)
##     free = free_symbols(ex)
##     vars = [free[:pop]()]
##     for i in 1:free[:__len__]()
##         push!(vars, free[:pop]())
##     end
##     len = length(vars)

##     if len == nvars
##         convert(Function, ex)
##     else
##         throw(DimensionMismatch("Expecting $nvars free variables and found $len"))
##     end
## end

## """
## Convert to a julia-valued function from R -> R::Float64
## """
## type ScalarFunction; end
## function Base.convert(::Type{ScalarFunction}, ex::Sym)
##     vars = free_symbols(ex)
##     length(vars) == 1 || error("Scalar function conversion is for expression of one variable")
##     u -> convert(Float64, N(subs(ex, vars[1], u)))
## end

convert(::Type{Function}, ex::Sym) = lambdify(ex)
## we usually promote to Sym objects, but here we want to promote to functions
## so [x, sin] -> will be plottable as two functions
Base.promote_rule{T<:SymbolicObject, S<:Function}(::Type{T}, ::Type{S} ) = S
