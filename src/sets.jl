## sets module
## TODO: SymSets class so that we can better manage dispatch?

"""
`SymPy` has the [sets](http://docs.sympy.org/latest/modules/sets.html) module that provides:

* `FiniteSet` to specify a finite set of values
* `Interval` to specify an interval
* `Union` to specify a union of finite sets and/or intervals.

The `FiniteSet` and `Interval` constructors are provided, unions are formed from these.

The `S` module provides some built-in sets: `S.Reals`, `S.Integers`, `S.Naturals`, `S.Naturals0`, and `S.UniversalSet`, `S.EmptySet`.

Various methods for sets and intervals are provided, though not all are defined for unions:

* `powerset`
* `union`, `intersection`, `contains`, `boundary`
* `complement`, `is_disjoint`, `is_subset`, `is_superset`, `is_proper_subset`
* `sup`, `inf`, `measure`
* `is_Union`, `is_Interval`, `is_FiniteSet`
* `as_relational`, `left_open`, `right_open`, `is_left_unbounded`, `is_right_unbounded`

Examples:
```
a,b = Interval(0,1), Interval(1//2,2, false, true)  # [0,1], [1/2, 2)
union(a, b)     # [0,2), also  a ∪ b
intersect(a,b)  # [1/2,1],  also a ∩ b
contains(a, 3/2), contains(b, 3/2) # false, true
inf(a), sup(b)  # 1, 2
right_open(b)   # true
complement(b, S.Reals)  # (-∞, 1/2) ∪ [2, ∞)
complement(b, a)  # [0, 1/2)  (This is a \ b, but that notation is not provided)

x = symbols("x")
as_relational(a, x)  #  0 ≤ x ∧ x ≤ 1

s = FiniteSet(1,2)
powerset(s) # {∅, {1}, {2}, {1, 2}}
```

SymPy `FiniteSet`s are not iterable, but can be collected with `collect(s.x)`

SymPy `Union`s are not iterable, but can be collected after calling `args` via `collect(args(s))`.

"""
sypmy_sets = nothing


module S
using SymPy
function init_set()
    S = sympy[:S]
    global Reals = S[:Reals]
    global UniversalSet = S[:UniversalSet]
    global Naturals = S[:Naturals]
    global Naturals0 = S[:Naturals0]
    global Integers = S[:Integers]
    global EmptySet = S[:EmptySet]
end
end


export S, FiniteSet, Interval, ProductSet, ConditionSet
export powerset, imageset
export contains,  boundary, sup, measure
export is_disjoint
export is_proper_subset, is_subset, is_superset
export Interval
export as_relational
export is_left_unbounded, is_right_unbounded, left_open, right_open
export is_FiniteSet, is_Union, is_Interval

"Convert a SymPy finite set to a Set of Sym objects"
function Base.convert(::Type{Set}, s::Sym)
    is_FiniteSet(s) || throw(ArgumentError("`s` must be a finite set"))
    s1 = Set(Any[u for u in s.x])
    for el in copy(s1)
        if !isa(el, Set) && !(el.x[:is_Symbol]) && is_FiniteSet(s) && length(el.x) > 1
            ## replace python FiniteSet with julian Set
            setdiff!(s1, Set([el]))
            push!(s1, Set(convert(Set, el)))
        end
    end
    s1
end

## to convert a Union to an array of intervals
function _union_to_intervals(s::Sym)
    is_Union(s) || throw(ArgumentError("`s` must be a Union of Intervals"))
    collect(args(s))
end
                         


"Find power set of set"
powerset(s::Sym) = s[:powerset]()



"Does I contain x?"
Base.contains(I::Sym, x) = (I[:contains](x) == Sym(true))
Base.in(x::Number, I::Sym) = contains(I, x)



"Complement of set within the universe"
Base.complement(I::Sym, U::Sym=S.Reals) = I[:complement](U)

"boundary, returnsa set"
boundary(I::Sym) = I.x[:boundary]

"Infinum of I"
Base.inf(I::Sym) = I.x[:inf]

"Intersection of two intervals"
Base.intersect(I::Sym, J::Sym) = I[:intersect](J)

"Are `I` and `J` disjoint?"
is_disjoint(I::Sym, J::Sym) = I[:is_disjoint](J) == Sym(true)

"Is `J` a proper subset of `I`?"
is_proper_subset(I::Sym, J::Sym) = I[:is_proper_subset](J) == Sym(true)

"Is `J` a  subset of `I`?"
is_subset(I::Sym, J::Sym) = I[:is_subset](J) == Sym(true)

"Is `I` a  superset of `J`?"
is_superset(I::Sym, J::Sym) = I[:is_superset](J) == Sym(true)

"Lebesgue mesuare of an interval"
measure(I::Sym) = I.x[:measure]




"Supremum of a set"
sup(I::Sym) = I.x[:sup]


"Union of two intervals"
Base.union(I::Sym, J::Sym) = I[:union](J)

"Symmetric difference of two intervals, in one or other, but not both: `(I ∪ J) \ (I  ∩ J)"
Base.symdiff(I::Sym, J::Sym) = complement(intersection(I,J), union(I,J))

"rexpress I in terms of relations involving variable `x`"
as_relational(I::Sym, x::Sym) = I[:as_relational](x)

# not implemented: `end` => `sup`, `left` =>

"Looks like (-oo, a)?"
is_left_unbounded(I::Sym) = I.x[:is_left_unbounded] == Sym(true)

"Looks like (a, oo)?"
is_right_unbounded(I::Sym) = I.x[:is_right_unbounded] == Sym(true)

"Looks like (a, b...?"
left_open(I::Sym) = I.x[:left_open] == Sym(true)

"Looks like ..., b)?"
right_open(I::Sym) = I.x[:right_open] == Sym(true)


"Is `I` a finite set?"
is_FiniteSet(I::Sym) = I.x[:is_FiniteSet]

"Is `I` an interval?"
is_Interval(I::Sym) = I.x[:is_Interval]

"Is `I` a union?"
is_Union(I::Sym) = I.x[:is_Union]

function init_sets()
    S.init_set()

    """
`FiniteSet(1,2,3)`

Create a finite set
"""
    "FiniteSet: http://docs.sympy.org/latest/modules/sets.html"
    global FiniteSet(args...) = sympy_meth(:FiniteSet, args...)

    "ProductSet: http://docs.sympy.org/latest/modules/sets.html"
    global ProductSet(args...) = sympy_meth(:ProductSet, args...)

    """
    Means to filter a set to pull out elements by a condition.

    ConditionSet:  A set of elements which satisfies a given condition.

    `ConditionSet(x, condition, S) = {x | condition(x) == true for x in S}`

    (Introduced in SymPy 1.0)
    """
    global ConditionSet(var::Sym, pred::Sym, S) = sympy_meth(:ConditionSet, var, pred, S)


    """
    Represent a ComplexRegion.
    working?
```    
    I, J = Interval(0,1), Interval(0,2)
    R = ComplexRegion(I * J)
    1 in R        # true
    2 + 1im in R  # false
```
"""
    ComplexRegion(IJ::Sym; kwargs...) = sympy_meth(:ComplexRegion, IJ; kwargs...)
    
    "imageset: http://docs.sympy.org/latest/modules/sets.html"
    global imageset(fn::Function, args...) = begin
        x = Sym("x")
        imageset(x, fn(x), args...)
    end
    global imageset(args...) = sympy_meth(:imageset, args...)

    
    ## Interval
    
"""
Create an interval object

```
Interval(0,1) # [0,1]
Interval(0,1,true, false) # (0,1]
```

"""
    global Interval(l,r,left_open=false, right_open=false) = sympy_meth(:Interval, Sym(l), Sym(r), left_open ,right_open)

    
end
