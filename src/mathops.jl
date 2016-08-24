## evaluate binary operations of symbolic objects
## XXX -- this may prove too narryw with the use of ::Sym
+{T<:SymbolicObject}(x::T, y::SymbolicObject) = pycall(sympy[:Add], Sym, x, y)::T
*{T<:SymbolicObject, S<:SymbolicObject}(x::T, y::S) = pycall(sympy[:Mul], Sym, x, y)::T
-{T<:SymbolicObject, S<:SymbolicObject}(x::T, y::S) = x + (-y)
-(x::SymbolicObject)                    =  (-1) * x 
/(x::Sym, y::Sym) = x * pycall(sympy[:Pow], Sym, y, -1)::Sym ## not SymbolicObject. Not sure why.
^(x::SymbolicObject, y::Rational) = x^convert(Sym,y)
^(x::Sym, y::Sym) = pycall(sympy[:Pow], Sym, x, y)::Sym      ## not SymbolicObject. Not sure why.
//(x::SymbolicObject, y::Int) = x / Sym(y)
//(x::SymbolicObject, y::Rational) = x / Sym(y)
//(x::SymbolicObject, y::SymbolicObject) = x / y

\(x::SymbolicObject, y::SymbolicObject) = (y'/x')'


inv(x::SymbolicObject) = x\one(x)
