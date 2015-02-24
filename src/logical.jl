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

@doc "This is `\ll<tab>` mapped as an infix operator to `Lt`" ->
(≪)(a::Sym, b::Sym) = Lt(a,b)  # \ll<tab>
Base.(:≤)(a::Sym, b::Sym) = Le(a,b)  # \le<tab>

@doc "For infix `Eq` one can use \Equal<tab> unicode operator" ->
(⩵)(a::Sym, b::Sym) = Eq(a,b)  # \Equal<tab>

@doc "use equality if a python level." ->
==(x::Sym, y::Sym) = x.x == y.x

Base.(:>=)(a::Sym, b::Sym) = Ge(a,b)
(≫)(a::Sym, b::Sym) = Gt(a,b)



export ≪,⩵,≫

<(x::Sym,  y::Sym) = PyCall.pyeval("x < y", x=project(x), y=project(y))
<(x::Sym, y::Number) = x < convert(Sym, y)
<=(x::Sym, y::Sym) = PyCall.pyeval("x <= y", x=project(x), y=project(y))
<=(x::Sym, y::Number) = x <= convert(Sym, y)
>=(x::Sym, y::Sym) = PyCall.pyeval("x >= y", x=project(x), y=project(y))
>=(x::Sym, y::Number) =  x >= convert(Sym, y)
>(x::Sym, y::Sym)  = PyCall.pyeval("x > y", x=project(x), y=project(y))
>(x::Sym, y::Number) = x > convert(Sym, y)
## hacky, but == is something else to SymPy
==(x::Sym, y::Number) = (x <= y) & (x >= y)

<(x::Number, y::Sym)  = y > x
<=(x::Number, y::Sym) = y >= x
>=(x::Number, y::Sym) = y <= x
>(x::Number, y::Sym)  = y < x

