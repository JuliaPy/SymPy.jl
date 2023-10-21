## functions for generic programming within SymPy
## if a generic method should apply to a sympy variable, it would
## be defined here.



## Iterator for Sym
import Base.iterate
iterate(x::Sym) = (x.__pyobject__, 0)
iterate(x::Sym, state) = nothing


Base.isless(a::Sym, b::Sym) = (a != sympy.nan && b != sympy.nan) && sympy.Lt(a,b) == Sym(true)
Base.isless(a::Sym, b::Number) = isless(promote(a,b)...)
Base.isless(a::Number, b::Sym) = isless(promote(a,b)...)

Base.isequal(a::Sym, b::Sym) = Eq(a,b) == Sym(true)
Base.isequal(a::Sym, b::Number) = Eq(promote(a,b)...) == Sym(true)
Base.isequal(a::Number, b::Sym) = Eq(promote(a,b)...) == Sym(true)


# Floating point bits
Base.eps(::Type{Sym}) = zero(Sym)
Base.eps(::Sym) = zero(Sym)
Base.signbit(x::Sym) = x < 0
Base.copysign(x::Sym, y::Number) = abs(x) * sign(y)
Base.flipsign(x::Sym, y) = signbit(y) ? -x : x
Base.typemax(::Type{Sym}) = oo
Base.typemin(::Type{Sym}) = -oo

Base.fld(x::SymbolicObject, y) = floor(x/y)
Base.cld(x::SymbolicObject, y) = ceil(x/y)
Base.mod(x::SymbolicObject, args...)= Mod(x, args...)
#Base.mod1
#Base.mod2pi
#Base.fldmod

# so we can compare numbers with ≈
Base.rtoldefault(::Type{<:SymbolicObject}) = eps()

function Base.round(x::Sym; kwargs...)
    length(free_symbols(x)) > 0 && throw(ArgumentError("can't round a symbolic expression"))
    round(N(x); kwargs...)
end

function Base.trunc(x::Sym; kwargs...)
    length(free_symbols(x)) > 0 && throw(ArgumentError("can't truncate a symbolic expression"))
    trunc(N(x); kwargs...)
end

# check on type of number
# these are boolean: true/false; not tru/false/nothing,as in SymPy
Base.isfinite(x::Sym) = !is_(:infinite, x)
Base.isinf(x::Sym) = is_(:infinite, x)
Base.isnan(x::Sym) = x == sympy.nan
Base.isinteger(x::Sym) = is_(:integer, x)
Base.iseven(x::Sym) = is_(:even, x)
Base.isreal(x::Sym) = is_(:real, x)
Base.isodd(x::Sym) = is_(:odd, x)




## zero and one (zeros?)
Base.zero(x::Sym) = Sym(0)
Base.zero(::Type{Sym}) = Sym(0)

Base.one(x::Sym) = Sym(1)
Base.one(::Type{Sym}) = Sym(1)




## float, complex, real, imag, angle
Base.float(x::Sym) = _float(N(x))
_float(x::Sym) = throw(ArgumentError("variable must have no free symbols"))
_float(x) = float(x)
Base.Float64(x::Sym) = _Float64(N(x))
_Float64(x::Sym) = throw(ArgumentError("variable must have no free symbols"))
_Float64(x) = Float64(x)

Base.Integer(x::Sym) = is_integer(x) ? N(x) : throw(DomainError("x can not be converted to an integer"))

Base.complex(::Type{Sym}) = Sym
Base.complex(r::Sym) = real(r) + imag(r) * im
function Base.complex(r::Sym, i)
    isreal(r) || throw(ArgumentError("r and i must not be complex"))
    isreal(i) || throw(ArgumentError("r and i must not be complex"))
    N(r) + N(i) * im
end
Base.complex(xs::AbstractArray{Sym}) = complex.(xs) # why is this in base?

Base.conj(x::SymbolicObject) = x.conjugate()
function Base.transpose(f::Sym)::Sym
    if pycall_hasproperty(PyObject(f), :transpose)
        f.transpose()
    else
        f
    end
end

Base.real(::Type{Sym}) = Sym
Base.real(x::Sym) = sympy.re(x)
Base.imag(x::Sym) =  sympy.im(x)

Base.angle(z::SymPy.SymbolicObject) = atan(sympy.im(z), sympy.re(z))


# sympy.div for poly division
Base.divrem(x::Sym, y) = sympy.div(x, y)
# needed for #390; but odd
Base.div(x::Sym, y::Union{Sym,Number}) = convert(Sym, sympy.floor(x/convert(Sym,y)))
Base.rem(x::Sym, y::Union{Sym,Number}) = x-Sym(y)*Sym(sympy.floor.(x/y))


Base.denominator(x::SymbolicObject) = denom(x)
Base.numerator(x::SymbolicObject) = numer(x)
