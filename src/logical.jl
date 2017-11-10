## Logical operators for (Sym,Sym)

# http://docs.sympy.org/dev/_modules/sympy/core/relational.html
logical_sympy_methods = (:Eq, :Ne, :Lt, :Le, :Gt, :Ge)

## these are defined for Reals, not Sym, Real...
## Necessary to lambdify Indicators as of v0.6.0-dev
## not exported
const relational_sympy_values = (:GreaterThan, :LessThan,
                                 :StrictGreaterThan, :StrictLessThan,
                                 :Equality, :Unequality)  
for meth in relational_sympy_values           
    meth_name = string(meth)
    @eval begin
        @doc """
`$($meth_name)`: a SymPy function. [cf.](http://docs.sympy.org/dev/_modules/sympy/core/relational.html)
""" ->
        ($meth)(a::Real, b::Real) = sympy_meth($meth_name,a, b)
    end
#    eval(Expr(:export, meth))
end




## XXX Experimental! Not sure these are such a good idea ...
## but used with piecewise
Base.:&(x::Sym, y::Sym) = PyCall.pycall(PyObject(x)["__and__"], Sym, y) 
Base.:|(x::Sym, y::Sym) =  PyCall.pycall(PyObject(x)["__or__"], Sym, y) 
!(x::Sym)         =       PyCall.pycall(x.x["__invert__"], Sym)::Sym 

## use ∨, ∧, ¬ for |,&,! (\vee<tab>, \wedge<tab>, \neg<tab>)
∨(x::Sym, y::Sym) = x | y
∧(x::Sym, y::Sym) = x & y
¬(x::Sym) = !x



## In SymPy, symbolic equations are not represented by `=` or `==`
## rather ther function `Eq` is used. Here we use the unicode
## `\Equal<tab>` for an infix operator. There are also unicode values to represent analogs of `<`, `<=`, `>=`. `>`. These are

## * `<`  is `\ll<tab>`
## * `<=` is `\leqq<tab>
## * `==` is `\Equal<tab>`
## * `>=` is `\geqq<tab>`
## * `>`  is `\gg<tab>`


## Instead we have: 
## We use unicode for visual appeal of infix operators, but the Lt, Le, Eq, Ge, Gt are the proper way:

"This is `\\ll<tab>` mapped as an infix operator to `Lt`"
(≪)(a::Sym, b::Sym) = Lt(a,b)  # \ll<tab>
(≪)(a::Sym, b::Number) = Lt(a,Sym(b)) # \ll<tab>
(≪)(a::Number, b::Sym) = Lt(Sym(a),b) # \ll<tab>

## could just do this, but it would interfere with other uses outside of SymPy
## (≪)(a::Number, b::Number) = Lt(promote(a,b)...)  # \ll<tab>

"This is `\\leqq<tab>` mapped as an infix operator to `Le`"
(≦)(a::Sym, b::Sym) = Le(a,b)   # \ll<tab>
(≦)(a::Sym, b::Number) = Le(a,Sym(b))  # \ll<tab>
(≦)(a::Number, b::Sym) = Le(Sym(a),b)   # \ll<tab>

"This is `\\gg<tab>` mapped as an infix operator to `Gt`"
(≫)(a::Sym, b::Sym) = Gt(a,b) |> asBool 
(≫)(a::Sym, b::Number) = Gt(a,Sym(b)) 
(≫)(a::Number, b::Sym) = Gt(Sym(a),b) 

"This is `\\geqq<tab>` mapped as an infix operator to `Ge`"
(≧)(a::Sym, b::Sym) = Ge(a,b) |> asBool 
(≧)(a::Sym, b::Number) = Ge(a,Sym(b))  
(≧)(a::Number, b::Sym) = Ge(Sym(a),b) 

"For infix `Eq` one can use \\Equal<tab> unicode operator"
(⩵)(a::Sym, b::Sym) = Eq(a,b)  # \Equal<tab>
(⩵)(a::Sym, b::Number) = Eq(a,Sym(b))  # \Equal<tab>
(⩵)(a::Number, b::Sym) = Eq(Sym(a),b)  # \Equal<tab>


export ≪,≦,⩵,≧,≫




"For hashing, we use equality at the python level."
==(x::Sym, y::Sym) = PyObject(x) == PyObject(y)  # but isequal is not the same!


## Question: what to do with comparisons between Sym and Sym which *can* be answered true or false?
## Here we throw an error when that is not the case in hopes that generic code can be called
## when decisions are possible.
## This is experimental

function asBool(x::Sym)
    x == SympyTRUE && return true
    x == !SympyTRUE && return false
    funcname(x) == "Equality" && return ==(args(x)...)

    convert(Bool, x)
#    throw(DomainError("Non Boolean value"))
end

Base.isless(a::Sym, b::Sym) = asBool(Lt(a,b))
Base.isless(a::Sym, b::Number) = asBool(Lt(promote(a,b)...))
Base.isless(a::Number, b::Sym) = asBool(Lt(promote(a,b)...))

Base.isequal(a::Sym, b::Sym) = asBool(Eq(a,b))
Base.isequal(a::Sym, b::Number) = asBool(Eq(promote(a,b)...))
Base.isequal(a::Number, b::Sym) = asBool(Eq(promote(a,b)...))

function !=(x::Sym, y::T)  where {T <: Real}
    try 
        x = convert(Float64, x)
        x != y
    catch
        true
    end
end
function !=(x::Sym, y::T) where {T <: Complex}
    try 
        x = complex(x)
        x != y
    catch
        true
    end
end

function init_logical()
    global const SympyTRUE = sympy_meth(:Lt, 0,1)
end
