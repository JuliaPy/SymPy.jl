

##################################################
## evaluate binary operations of symbolic objects
## XXX -- this may prove too narryw with the use of ::Sym

+(x::SymbolicObject, y::SymbolicObject) = x.__add__(y)
*(x::SymbolicObject, y::SymbolicObject) = x.__mul__(y)

-(x::SymbolicObject, y::SymbolicObject) = x.__sub__(y)
-(x::SymbolicObject)                    = x.__neg__()
/(x::SymbolicObject, y::SymbolicObject) = x.__div__(y)
^(x::SymbolicObject, y::SymbolicObject) = x.__pow__(y)
^(x::SymbolicObject, y::Rational) = x^convert(Sym,y)
#^(x::SymbolicObject, y::Integer) = x^convert(Sym,y) # no Union{Integer, Rational}, as that has ambiguity
//(x::SymbolicObject, y::Int) = x / Sym(y)
//(x::SymbolicObject, y::Rational) = x / Sym(y)
//(x::SymbolicObject, y::SymbolicObject) = x / y

\(x::SymbolicObject, y::SymbolicObject) = (y'/x')' # ?

inv(x::Sym) = x.__pow__(Sym(-1))
