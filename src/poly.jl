## There are many Poly class methods
## Instances of Poly can be created via:
## x, y, z = @syms x y z
## p = poly(x^2 -x - 1)
##
## Is it wrapping their methods, e.g.
## all_coeffs(p) = object_meth(p, :all_coeffs) | u -> [convert(Sym, x) for x in u] | float?
## 

# ## XX Need to have a Sym abstract class it seems to do this kind of thing
# immutable SymPoly
#     x::PyCall.PyObject
# end

# project(x::SymPoly) = x.x
#convert(::Type{Sym}, x::SymPoly) = Sym(x.x)
#convert(::Type{SymPoly}, x::Sym) = SymPoly(x.x)

const SymPoly = Sym



poly(s::Sym, args...; kwargs...) = convert(SymPoly, call_meth(:poly, s, args...; kwargs...))

## polynomial divide
function div(f::SymPoly, g::SymPoly, args...; kwargs...)
    ans = sympy_meth(:div, f, g, args...; kwargs...)
    map(u -> convert(Sym, u), ans) # q,r
end

## list -> float
function real_roots(s::SymPoly) 
    "Return a list of real roots with multiplicities of f."
    ans = sympy_meth(:real_roots, s)
    [convert(Sym, u) for u in ans] | float
end

## list -> list (may be complex, real, ...)
function nroots(s::SymPoly) 
    "Compute numerical approximations of roots of f."
    ans = sympy_meth(:nroots, s)
    [convert(Sym, u) for u in ans]
end

## -> integer
function count_roots(s::SymPoly, args...) 
    sympy_meth(:count_roots, s, args...) | integer
end

## -> integer    
content(s::SymPoly) = sympy_meth(:content, s) | integer
