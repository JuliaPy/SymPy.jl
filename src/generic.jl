## functions for generic programming within SymPy
## if a generic method should apply to a sympy variable, it would
## be defined here.



## Iterator for Sym
import Base.iterate
iterate(x::Sym) = (x.x, 0)
iterate(x::Sym, state) = nothing


Base.isless(a::Sym, b::Sym) = sympy.Lt(a,b) == True
Base.isless(a::Sym, b::Number) = sympy.Lt(promote(a,b)...) == True
Base.isless(a::Number, b::Sym) = sympy.Lt(promote(a,b)...) == True

Base.isequal(a::Sym, b::Sym) = Eq(a,b) == True
Base.isequal(a::Sym, b::Number) = Eq(promote(a,b)...) == True
Base.isequal(a::Number, b::Sym) = Eq(promote(a,b)...) == True


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




## float, complex, real, image,
Base.float(x::Sym) = _float(N(x))
_float(x::Sym) = throw(ArgumentError("variable must have no free symbols"))
_float(x) = float(x)
Base.Float64(x::Sym) = _Float64(N(x))
_Float64(x::Sym) = throw(ArgumentError("variable must have no free symbols"))
_Float64(x) = Float64(x)


Base.complex(::Type{Sym}) = Sym
Base.complex(r::Sym) = real(r) + imag(r) * im
function Base.complex(r::Sym, i)
    isreal(r) || throw(ArgumentError("r and i must not be complex"))
    isreal(i) || throw(ArgumentError("r and i must not be complex"))
    N(r) + N(i) * im
end
Base.complex(xs::AbstractArray{Sym}) = complex.(xs) # why is this in base?

Base.conj(x::SymbolicObject) = x.conjugate()


Base.real(::Type{Sym}) = Sym
Base.real(x::Sym) = _real(N(x))
_real(x::Sym) = sympy.:re(x)
_real(x) = real(x)



Base.imag(x::Sym) = _imag(N(x))
_imag(x::Sym) = sympy.im(x)
_imag(x) = imag(x)




# sympy.div for poly division
Base.div(x::Sym, y::Union{Sym,Number}) = convert(Sym, sympy.floor(x/convert(Sym,y)))
Base.rem(x::Sym, y::Union{Sym,Number}) = x-Sym(y)*Sym(sympy.floor.(x/y))

Base.denominator(x::SymbolicObject) = denom(x)
Base.numerator(x::SymbolicObject) = numer(x)
