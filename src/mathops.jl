## evaluate binary operations of symbolic objects
## XXX -- this may prove too narryw with the use of ::Sym
+(x::SymbolicObject, y::SymbolicObject) = pycall(sympy["Add"], Sym, x, y)
*(x::SymbolicObject, y::SymbolicObject) = pycall(sympy["Mul"], Sym, x, y)
-(x::SymbolicObject, y::SymbolicObject) = x + (-y)
-(x::SymbolicObject)                    =  (-1) * x
/(x::Sym, y::Sym) = x * pycall(sympy["Pow"], Sym, y, -1)::Sym
^(x::SymbolicObject, y::Rational) = x^convert(Sym,y)
^(x::SymbolicObject, y::Integer) = x^convert(Sym,y) # no Union{Integer, Rational}, as that has ambiguity
^(x::Sym, y::Sym) = pycall(sympy["Pow"], Sym, x, y)::Sym      
//(x::SymbolicObject, y::Int) = x / Sym(y)
//(x::SymbolicObject, y::Rational) = x / Sym(y)
//(x::SymbolicObject, y::SymbolicObject) = x / y

\(x::SymbolicObject, y::SymbolicObject) = (y'/x')' # ?


inv(x::SymbolicObject) = x\one(x)
