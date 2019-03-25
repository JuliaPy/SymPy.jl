

##################################################
## evaluate binary operations of symbolic objects
## XXX -- this may prove too narryw with the use of ::Sym

+(x::SymbolicObject, y::SymbolicObject) = pycall(sympy.Add, Sym, x, y)
*(x::SymbolicObject, y::SymbolicObject) = pycall(sympy.Mul, Sym, x, y)
-(x::SymbolicObject, y::SymbolicObject) = x + (-y)
-(x::SymbolicObject)                    =  (-1) * x
/(x::SymbolicObject, y::SymbolicObject) = x * inv(y)
^(x::SymbolicObject, y::SymbolicObject) = pycall(sympy.Pow, Sym, x, y)::Sym
^(x::SymbolicObject, y::Rational) = x^convert(Sym,y)
#^(x::SymbolicObject, y::Integer) = x^convert(Sym,y) # no Union{Integer, Rational}, as that has ambiguity
//(x::SymbolicObject, y::Int) = x / Sym(y)
//(x::SymbolicObject, y::Rational) = x / Sym(y)
//(x::SymbolicObject, y::SymbolicObject) = x / y

\(x::SymbolicObject, y::SymbolicObject) = (y'/x')' # ?


#inv(x::Sym) = x\one(x)
#inv(x::Sym) = x^(-1)
inv(x::Sym) = pycall(sympy.Pow, Sym, x, -1)
