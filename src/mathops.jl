## Numbers. Should be a better way to make a value numeric
function convert{T <: Real}(::Type{Sym}, x::T)
    a = Sym(randstring(10))
    a |> (a == x)
end


## Math Ops. May have this wrong.
## need to call through with pyeval here
+(x::Sym, y::Sym) =  Sym(pyeval("x + y", x = project(x), y = project(y)))
+(x::Sym, y::Real) = Sym(pyeval("x + y", x = project(x), y = project(y)))
+(x::Real, y::Sym) = Sym(pyeval("x + y", x = project(x), y = project(y)))
+(a::Array, x::Sym) = map(u -> u + x, a)
+(x::Sym, a::Array) = map(u -> x + u, a)

-(x::Sym, y::Sym) =  Sym(pyeval("x - y", x = project(x), y = project(y)))
-(x::Sym, y::Real) = Sym(pyeval("x - y", x = project(x), y = project(y)))
-(x::Real, y::Sym) = Sym(pyeval("x - y", x = project(x), y = project(y)))
-(a::Array, x::Sym) = map(u -> u - x, a)
-(x::Sym, a::Array) = map(u -> x - u, a)


-(x::Sym) = Sym(pyeval("-x", x = project(x)))

*(x::Sym, y::Sym) =  Sym(pyeval("x * y", x = project(x), y = project(y)))
*(x::Sym, y::Real) = Sym(pyeval("x * y", x = project(x), y = project(y)))
*(x::Real, y::Sym) = Sym(pyeval("x * y", x = project(x), y = project(y)))
*(x::Sym, a::Array) = map(u -> x*u, a)
*(a::Array, x::Sym) = map(u -> x*u, a)
.*{T <: Number}(x::T, y::Sym) = convert(Sym, x) * y
.*{T <: Number}(x::Sym, y::T) = x * convert(Sym, y)
.*(x::Sym, y::Sym) = convert(Sym, convert(Array{Sym}, x) .* convert(Array{Sym}, y))
.*(x::Sym, a::Array) = map(u -> x*u, a)
.*(a::Array, x::Sym) = map(u -> x*u, a)

/(x::Sym, y::Sym) =  Sym(pyeval("x / y", x = project(x), y = project(y)))
/(x::Sym, y::Real) = Sym(pyeval("x / y", x = project(x), y = project(y)))
/(x::Real, y::Sym) = Sym(pyeval("x / y", x = project(x), y = project(y)))
/(a::Array, x::Sym) = map(u -> u/x, a)
./(a::Array, x::Sym) = map(u -> u/x, a)
./(x::Sym, y::Sym) = convert(Sym, convert(Array{Sym}, x) ./ convert(Array{Sym}, y))
./{T <: Number}(x::T, y::Sym) = convert(Sym, x) / y
./{T <: Number}(x::Sym, y::T) = x / convert(Sym, y)

^(x::Sym, y::Sym) =     Sym(pyeval("x ** y", x = project(x), y = project(y)))
^(x::Sym, y::Integer) = Sym(pyeval("x ** y", x = project(x), y = project(y)))
^(x::Sym, y::Real) =    Sym(pyeval("x ** y", x = project(x), y = project(y)))
^(x::Real, y::Sym) =    Sym(pyeval("x ** y", x = project(x), y = project(y)))
.^(x::Sym, y::Sym) = convert(Array{Sym}, x) .^ convert(Array{Sym}, y)
.^{T <: Number}(x::T, y::Array{Sym}) = map(u -> x^u, y)
.^{T <: Number}(x::Array{Sym}, y::T) = map(u -> u^y, x)


