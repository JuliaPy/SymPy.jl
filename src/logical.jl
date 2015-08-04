## Logical operators for (Sym,Sym)

## Experimental! Not sure these are such a good idea ...
## but used with piecewise
Base.&(x::Sym, y::Sym) = PyCall.pyeval("x & y", x=project(x), y=project(y))
Base.|(x::Sym, y::Sym) = PyCall.pyeval("x | y", x=project(x), y=project(y))
!(x::Sym)         =      PyCall.pyeval("~x",    x=project(x))

## use ∨, ∧, ¬ for |,&,! (\vee<tab>, \wedge<tab>, \neg<tab>)
∨(x::Sym, y::Sym) = x | y
∧(x::Sym, y::Sym) = x & y
¬(x::Sym) = !x


"""

In SymPy, symbolic equations are not represented by `=` or `==`,
rather ther function `Eq` is used. Here we use the unicode
`\Equal<tab>` for an infix operator. There are also unicode values to represent `<`, `<=`, `>=`. `>`. These are

* `<` is `\ll<tab>`
* `<=` is `\le<tab>
* `==` is `\Equal<tab>`
* `>=` is `\ge<tab>`
* `>` is `\gg<tab>`

"""

## These are useful with pairwise and plot_implicit, but they cause  issues elsewhere with linear algebra functions
#Base.isless(a::Sym, b::Sym) = Lt(a,b)
#Base.isequal(a::Sym, b::Sym) = Eq(a,b)
# ⩵(a::Sym, b::Sym) = isequal(a,b)
#export ⩵
## Instead we have:
## We use unicode for visual appeal of infix operators, but the Lt, Le, Eq, Ge, Gt are the proper way:

"This is `\ll<tab>` mapped as an infix operator to `Lt`"
(≪)(a::Sym, b::Sym) = Lt(a,b)  # \ll<tab>
(≪)(a::Sym, b::Number) = Lt(a,Sym(b))  # \ll<tab>
(≪)(a::Number, b::Sym) = Lt(Sym(a),b)  # \ll<tab>

Base.(:≤)(a::Sym, b::Sym) = Le(a,b)  # \le<tab>
Base.(:≤)(a::Sym, b::Number) = Le(a,Sym(b))  # \le<tab>
Base.(:≤)(a::Number, b::Sym) = Le(Sym(a),b)  # \le<tab>

"For hashing, we use equality at the python level."
## Base.isequal(x::Sym, y::Sym) = x.x == y.x
==(x::Sym, y::Sym) = x.x == y.x

"For infix `Eq` one can use \Equal<tab> unicode operator"
(⩵)(a::Sym, b::Sym) = Eq(a,b)  # \Equal<tab>
(⩵)(a::Sym, b::Number) = Eq(a,Sym(b))  # \Equal<tab>
(⩵)(a::Number, b::Sym) = Eq(Sym(a),b)  # \Equal<tab>


Base.(:>=)(a::Sym, b::Sym) = Ge(a,b)
Base.(:>=)(a::Sym, b::Number) = Ge(a,Sym(b))
Base.(:>=)(a::Number, b::Sym) = Ge(Sym(a),b)


(≫)(a::Sym, b::Sym) = Gt(a,b)
(≫)(a::Sym, b::Number) = Gt(a,Sym(b))
(≫)(a::Number, b::Sym) = Gt(Sym(a),b)

export ≪,⩵,≫


# ## XXX need a decision.
# ## These are useful for `piecewise` and `plot_implicit`, but mess up generic code
# ## we likely only need to define isequal, ==, isless and < for generic code, but there is
# ## no way for < to return a boolean in general, save for (x < y -> float(x) < float(y)???)

## Try this. It works with `sort`
const SympyTRUE = sympy.Lt(0,1)
Base.isless(x::Sym, y::Sym) = sympy.Lt(x.x, y.x) == SympyTRUE

# <(x::Sym,  y::Sym) = Lt(x,y)
# <(x::Sym, y::Number) = x < convert(Sym, y)
# <=(x::Sym, y::Sym) = Le(x,y)
# <=(x::Sym, y::Number) = x <= convert(Sym, y)
# >=(x::Sym, y::Sym) = Ge(x,y)
# >=(x::Sym, y::Number) =  x >= convert(Sym, y)
# >(x::Sym, y::Sym)  = Gt(x,y)
# >(x::Sym, y::Number) = x > convert(Sym, y)
# ## hacky, but == is something else to SymPy. (This is \Equal<tab>
# ==(x::Sym, y::Number) = Eq(x,y) #(x <= y) & (x >= y)

# <(x::Number, y::Sym)  = y > x
# <=(x::Number, y::Sym) = y >= x
# >=(x::Number, y::Sym) = y <= x
# >(x::Number, y::Sym)  = y < x

