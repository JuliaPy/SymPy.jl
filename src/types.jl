## Code for our Types and conversion methods


## Symbol class for controlling dispatch
abstract SymbolicObject <: Number

## Basic types defined here
immutable Sym <: SymbolicObject
    x::PyCall.PyObject
end
Sym(s::SymbolicObject) = s

## Matrix type
immutable SymMatrix <: SymbolicObject
    x::PyCall.PyObject
end

## complex float
## this cause issue with printing on non-complex objects
#mpctype = sympy.mpmath["ctx_mp_python"]
#pytype_mapping(mpctype, Sym)

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
    if VERSION >= v"0.4.0-dev"
        ntuple(i -> o[:__getitem__](i-1), n)
    else
        ntuple(n, i -> o[:__getitem__](i-1))
    end
end

## rational
convert{T<:SymbolicObject}(::Type{T}, x::Rational) = sympy_meth(:Rational, x.num, x.den)

## big. Need mpmath installed separately -- not as a SymPy module as that is how it is called in PyCall
convert{T<:SymbolicObject}(::Type{T}, x::BigFloat) = Sym(PyCall.PyObject(x))
convert(::Type{Sym}, x::Complex{BigFloat}) = Sym(PyCall.PyObject(x))

## real
convert{S<:SymbolicObject, T <: Real}(::Type{S}, x::T) = sympy_meth(:sympify, x)
convert{T <: Real}(::Type{T}, x::Sym) = convert(T, project(x))


## complex
## Sym(PyCall.PyObject(im)) gives 1j, not i (One is python, the other SymPy)
function convert(::Type{Sym}, x::Complex)
    if isa(x, Complex{Bool})
        return(Sym(sympy[:I]))
    end
    if real(x) == 0
        Sym(sympy[:Symbol]("$(imag(x))*I"))
    else
        Sym(sympy[:Symbol]("$(real(x)) + $(imag(x))*I"))
    end
end
convert(::Type{Complex}, x::Sym) = complex(map(x -> convert(Float64, x), x[:as_real_imag]())...)
complex(x::Sym) = convert(Complex, x)
complex(xs::Array{Sym}) = map(complex, xs)

## matrices
convert(::Type{SymMatrix}, o::PyCall.PyObject) = SymMatrix(o)
convert(::Type{Sym}, o::SymMatrix) = Sym(o.x)
convert(::Type{SymMatrix}, o::Sym) = SymMatrix(o.x)

## string
convert(::Type{Sym}, o::String) = sympy_meth(:sympify, o)
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
function convert(::Type{Function}, ex::Sym)
    vars = get_free_symbols(ex)
    len = length(vars)
    if len == 0
        return x -> ex
    end
    local out
    (args...) -> begin
        out = ex
        for i in 1:len
            out = object_meth(out, :subs, vars[i], args[i]) #convert(Function, out[:subs])(project(vars[i]), args[i])
        end
        out
    end
end

## For plotting we need to know if a function has 1, 2, or 3 free variables
function as_nfunction(ex::Sym, nvars=1)
    free = free_symbols(ex)
    vars = [free[:pop]()]
    for i in 1:free[:__len__]()
        push!(vars, free[:pop]())
    end
    len = length(vars)

    if len == nvars
        convert(Function, ex)
    else
        throw(DimensionMismatch("Expecting $nvars free variables and found $len"))
    end
end

