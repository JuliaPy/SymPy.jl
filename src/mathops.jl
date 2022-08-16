

##################################################
## evaluate binary operations of symbolic objects
## XXX -- this may prove too narryw with the use of ::Sym



+(x::Sym, y::Sym)::Sym = x.__add__(y)
*(x::Sym, y::Sym)::Sym = x.__mul__(y)
-(x::Sym, y::Sym)::Sym = x.__sub__(y)
(-)(x::Sym)::Sym                    = x.__neg__()
/(x::Sym, y::Sym)::Sym = x.__truediv__(y)
^(x::Sym, y::Sym)::Sym = x.__pow__(y)
^(x::Sym, y::Rational) = x^convert(Sym,y)
#^(x::Sym, y::Integer) = x^convert(Sym,y) # no Union{Integer, Rational}, as that has ambiguity
//(x::Sym, y::Int) = x / Sym(y)
//(x::Sym, y::Rational) = x / Sym(y)
//(x::Sym, y::Sym) = x / y

\(x::Sym, y::Sym) = (y'/x')' # ?

Base.inv(x::Sym) = x.__pow__(Sym(-1))

# special case Boolean; issue   351
# promotion for Boolean here is to 0 or  1,  not False,  True
+(x::Bool, y::Sym)::Sym = Sym(Int(x)).__add__(y)
*(x::Bool, y::Sym)::Sym = Sym(Int(x)).__mul__(y)
-(x::Bool, y::Sym)::Sym = Sym(Int(x)).__sub__(y)
/(x::Bool, y::Sym)::Sym = Sym(Int(x)).__truediv__(y)
^(x::Bool, y::Sym)::Sym = Sym(Int(x)).__pow__(y)
+(x::Sym, y::Bool)::Sym = x.__add__(Int(y))
*(x::Sym, y::Bool)::Sym = x.__mul__(Int(y))
-(x::Sym, y::Bool)::Sym = x.__sub__(Int(y))
/(x::Sym, y::Bool)::Sym = x.__truediv__(Int(y))
^(x::Sym, y::Bool)::Sym = x.__pow__(Int(y))
