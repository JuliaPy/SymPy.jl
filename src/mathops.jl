## evaluate binary operations of symbolic objects
+(x::SymbolicObject, y::SymbolicObject) =  pyeval("x + y", x = project(x), y = project(y))
-(x::SymbolicObject, y::SymbolicObject) =  pyeval("x - y", x = project(x), y = project(y))
-(x::SymbolicObject)                    =  pyeval("-x"   , x = project(x))
*(x::SymbolicObject, y::SymbolicObject) =  pyeval("x * y", x = project(x), y = project(y))
/(x::SymbolicObject, y::SymbolicObject) =  pyeval("x / y", x = project(x), y = project(y))
^(x::SymbolicObject, y::SymbolicObject) =  pyeval("x ** y", x = project(x), y = project(y))
^(x::SymbolicObject, y::Rational) = x^convert(Sym,y)

\(x::SymbolicObject, y::SymbolicObject) = (y'/x')'
# \(x::SymbolicObject, y::Number) = x\convert(Sym, y)
# \(x::Number, y::SymbolicObject) = convert(Sym, x) \ y
# \(a::Array, x::SymbolicObject) = map(u -> u\x, a)
# .\(a::Array, x::SymbolicObject) = map(u -> u\x, a)
# .\(x::SymbolicObject, y::SymbolicObject) = convert(Sym, convert(Array{Sym}, x) .\ convert(Array{Sym}, y))
# .\{T <: Number}(x::T, y::SymbolicObject) = convert(Sym, x) \ y
# .\{T <: Number}(x::SymbolicObject, y::T) = x \ convert(Sym, y)

inv(x::SymbolicObject) = x\one(x)
