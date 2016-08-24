## Logical operators for (Sym,Sym)

## XXX Experimental! Not sure these are such a good idea ...
## but used with piecewise
@compat Base.:&(x::Sym, y::Sym) = PyCall.pycall(x.x["__and__"], Sym, y) #PyCall.pyeval("x & y", Sym, x=project(x), y=project(y))::Sym
@compat Base.:|(x::Sym, y::Sym) =  PyCall.pycall(x.x["__or__"], Sym, y) #PyCall.pyeval("x | y", Sym, x=project(x), y=project(y))::Sym
!(x::Sym)         =       PyCall.pycall(x.x["__invert__"], Sym)::Sym #Sym( PyCall.pyeval("~x",  Sym,  x=project(x)))::Sym

## use ∨, ∧, ¬ for |,&,! (\vee<tab>, \wedge<tab>, \neg<tab>)
∨(x::Sym, y::Sym) = x | y
∧(x::Sym, y::Sym) = x & y
¬(x::Sym) = !x


#=

In SymPy, symbolic equations are not represented by `=` or `==`,
rather ther function `Eq` is used. Here we use the unicode
`\Equal<tab>` for an infix operator. There are also unicode values to represent `<`, `<=`, `>=`. `>`. These are

* `<` is `\ll<tab>`
* `<=` is `\le<tab>
* `==` is `\Equal<tab>`
* `>=` is `\ge<tab>`
* `>` is `\gg<tab>`

=#

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

## compat for v0.4
@compat Base.:≤(a::Sym, b::Sym) = Le(a,b)  # \le<tab>
@compat Base.:≤(a::Sym, b::Number) = Le(a,Sym(b))  # \le<tab>
@compat Base.:≤(a::Number, b::Sym) = Le(Sym(a),b)  # \le<tab>

"For hashing, we use equality at the python level."
## Base.isequal(x::Sym, y::Sym) = x.x == y.x
==(x::Sym, y::Sym) = x.x == y.x

"For infix `Eq` one can use \Equal<tab> unicode operator"
(⩵)(a::Sym, b::Sym) = Eq(a,b)  # \Equal<tab>
(⩵)(a::Sym, b::Number) = Eq(a,Sym(b))  # \Equal<tab>
(⩵)(a::Number, b::Sym) = Eq(Sym(a),b)  # \Equal<tab>


# compat for v0.4
@compat Base.:>=(a::Sym, b::Sym) = Ge(a,b)
@compat Base.:>=(a::Sym, b::Number) = Ge(a,Sym(b))
@compat Base.:>=(a::Number, b::Sym) = Ge(Sym(a),b)


(≫)(a::Sym, b::Sym) = Gt(a,b)
(≫)(a::Sym, b::Number) = Gt(a,Sym(b))
(≫)(a::Number, b::Sym) = Gt(Sym(a),b)

export ≪,⩵,≫


# ## XXX need a decision.
# ## These are useful for `piecewise` and `plot_implicit`, but mess up generic code
# ## we likely only need to define isequal, ==, isless and < for generic code, but there is
# ## no way for < to return a boolean in general, save for (x < y -> float(x) < float(y)???)


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


function !={T <: Real}(x::Sym, y::T) 
    try 
        x = convert(Float64, x)
        x != y
    catch
        true
    end
end
function !={T <: Complex}(x::Sym, y::T) 
    try 
        x = complex(x)
        x != y
    catch
        true
    end
end

function init_logical()
    ## Try this. It works with `sort`
    global const SympyTRUE = sympy_meth(:Lt, 0,1)
    Base.isless(x::Sym, y::Sym) = sympy_meth(:Lt,x, y) == SympyTRUE
end
