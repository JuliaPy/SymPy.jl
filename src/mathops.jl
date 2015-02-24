## evaluate binary operations of symbolic objects
+(x::SymbolicObject, y::SymbolicObject) =  pyeval("x + y", x = project(x), y = project(y))
-(x::SymbolicObject, y::SymbolicObject) =  pyeval("x - y", x = project(x), y = project(y))
-(x::SymbolicObject)                    =  pyeval("-x"   , x = project(x))
*(x::SymbolicObject, y::SymbolicObject) =  pyeval("x * y", x = project(x), y = project(y))
/(x::SymbolicObject, y::SymbolicObject) =  pyeval("x / y", x = project(x), y = project(y))
^(x::SymbolicObject, y::SymbolicObject) =  pyeval("x ** y", x = project(x), y = project(y))
^(x::SymbolicObject, y::Rational) = x^convert(Sym,y)
