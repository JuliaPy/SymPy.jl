convert{T <: Real}(::Type{Sym}, x::T) = sympy.sympify(x)


## Math Ops. May have this wrong, as should work through promotion rules
## without all these cases.
## need to call through with pyeval here
+(x::SymbolicObject, y::SymbolicObject) =  Sym(pyeval("x + y", x = project(x), y = project(y)))
+(x::SymbolicObject, y::Number) = x + convert(Sym, y)
+(y::Number, x::SymbolicObject) = x + y
+(a::Array, x::SymbolicObject) = map(u -> u + x, a)
+(x::SymbolicObject, a::Array) = map(u -> x + u, a)

-(x::SymbolicObject, y::SymbolicObject) =  Sym(pyeval("x - y", x = project(x), y = project(y)))
-(x::SymbolicObject, y::Number) = x - convert(Sym, y)
-(y::Number, x::SymbolicObject) = -(x-y)
-(a::Array, x::SymbolicObject) = map(u -> u - x, a)
-(x::SymbolicObject, a::Array) = map(u -> x - u, a)

-(x::SymbolicObject) = Sym(pyeval("-x", x = project(x)))

*(x::SymbolicObject, y::SymbolicObject) =  Sym(pyeval("x * y", x = project(x), y = project(y)))
*(x::SymbolicObject, y::Number) = x * convert(Sym, y)
*(x::Number, y::SymbolicObject) = y * x
*(x::SymbolicObject, a::Array) = map(u -> x*u, a)
*(a::Array, x::SymbolicObject) = map(u -> x*u, a)
.*{T <: Number}(x::T, y::SymbolicObject) = convert(Sym, x) * y
.*{T <: Number}(x::SymbolicObject, y::T) = x * convert(Sym, y)
.*(x::SymbolicObject, y::SymbolicObject) = convert(Sym, convert(Array{Sym}, x) .* convert(Array{Sym}, y))
.*(x::SymbolicObject, a::Array) = map(u -> x*u, a)
.*(a::Array, x::SymbolicObject) = map(u -> x*u, a)

/(x::SymbolicObject, y::SymbolicObject) =  Sym(pyeval("x / y", x = project(x), y = project(y)))
/(x::SymbolicObject, y::Number) = x/convert(Sym, y)
/(x::Number, y::SymbolicObject) = convert(Sym, x) / y
/(a::Array, x::SymbolicObject) = map(u -> u/x, a)
./(a::Array, x::SymbolicObject) = map(u -> u/x, a)
./(x::SymbolicObject, y::SymbolicObject) = convert(Sym, convert(Array{Sym}, x) ./ convert(Array{Sym}, y))
./{T <: Number}(x::T, y::SymbolicObject) = convert(Sym, x) / y
./{T <: Number}(x::SymbolicObject, y::T) = x / convert(Sym, y)

^(x::SymbolicObject, y::SymbolicObject) =     Sym(pyeval("x ** y", x = project(x), y = project(y)))
^(x::SymbolicObject, y::Integer) = x^convert(Sym, y)
^(x::SymbolicObject, y::Number) =    Sym(pyeval("x ** y", x = project(x), y = project(y)))
^(x::Real, y::SymbolicObject) =   convert(Sym,x)^y
.^(x::SymbolicObject, y::SymbolicObject) = convert(Array{Sym}, x) .^ convert(Array{Sym}, y)
#.^{T <: Number}(x::T, y::Array{Sym}) = map(u -> x^u, y) # conflict with e^[...] so don't define
.^{T <: Number}(x::Array{Sym}, y::T) = map(u -> u^y, x)


