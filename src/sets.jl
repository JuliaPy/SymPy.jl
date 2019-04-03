## sets


## for  finite set Set(s...) will work too, but
## may have wider type, e.g. Any
function Base.convert(::Type{Set}, s::Sym)
    is_(:FiniteSet,s) || throw(ArgumentError("`s` must be a finite set"))
    s1 = Set(u for u in s.x)

    s1
end

"""
    elements(s)

return elements of a set s as an array, unlike `convert(Set,s)`
"""
elements(s::Sym) = collect(convert(Set, s))
export elements

# is x in set; avoid ambiguity
Base.in(x::Sym, I::Sym) = I.contains(x) == Sym(true)
Base.in(x::Number, I::Sym) = Sym(x) in I
