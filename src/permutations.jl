function _check_permutation_format(x::Vector{Vector{T}}) where {T}
    nothing
end
_check_permutation_format(x) = nothing

"""

    Permutation(args...)

This module mostly implements SymPy's [Permuation](http://docs.sympy.org/latest/modules/combinatorics/permutations.html) module.

A permuation can be represented in different ways. Here a permutation is a reaarangment of the values 0, 1, ..., n. For example, the mapping `0->2, 1->3, 2->0, 3->1` can be presented by a vector: `sigma = [2,3,0,1]` where `sigma[i] = j` when `i -> j`. Otheriwse, it can be presented as a product of cycles: `(0 2)(1 3)` which reads 0 goes to 2 which goes to 0 (wrapping) and 1 goes to 3 and 3 goes to 1.

Either representation can be passed through the `Permutation` constructor.

For the vector notation -- 0-based -- it is passed directly to the constructor:

```
p = Permuation([2,3,0,1])
```

If a range describes a permutation, it can be used as well:

```
id = Permutation(0:10)
```

Cycle notation can more compactly describe a permuation, it can be passed in as a container of cycles specified through tuples or vectors:

```
p = Permutation([(0,2), (1,3)])
```

The latter can be be expresed more quickly as

```
p = Permutation(0,2)(1,3)
```

This works as a single cycle can be passed to the `Permutation`
constructor with values separated by commas and the "call" method for
`Permuation` objects is overloaded: for a single argument, the mapping
`i -> j` is created (also the notation `i^p` returns this`) *but* if
more than one argument is given, a cycle is created and multiplied on
the *right* by `p`, so that the above becomes `(0,2) * (1,3)`.

Here are two permutations forming the symmetries of square, naturally represented in the two ways:

```
flip = Permutation([[0,1],[2,3]])  # or Permutation(0,1)(2,3)
rotate = Permutation([1,2,3,0])    # or Permutation(0,1,2,3) in cycle notation
```

Operations on permutations include:

* a function call, `p(i)` to recover `j` where `i -> j`, also `i^p`.
* `*` for multiplication. The convention is `(p*q)(i) = q(p(i))` or with the `^` notation: `i^(p*q) = (i^p)^q`.
* `+` for multiplication when `p` and `q` commute, where a check on commuting is performed.
* `inv` for the inverse permutation.
* `/`, where `p/q` is `p * inv(q)`.
* `p^n` for powers. We have `inv(p) = p^(-1)` and `p^order(p)` is the identity.
* `p^q` for conjugate, defined by `inv(q) * p * q`.


We can see that a flip is an involution through:

```
flip^2  # the identity
```

wheres a rotation is not (as it has order 4)

```
rotate * rotate
rotate.order()
```

These do not commute:

```
flip * rotate  # (3)(0 2) -- note (n) is the identity
```

```
rotate * flip  # (1 3)
```

We can see this is the correct mapping `1 -> 3` with

```
(1^rotate)^flip, 1^(rotate*flip), flip(rotate(1))
```

We can check that `flip` and `rotate^2` do commute:

```
id = Permutation(3)   # (n) is the identify
flip.commutator(rotate^2) == id
```

The conjugate for flip and rotate does the inverse of the flip, then rotates, then flips:

```
rotate^flip
```

This is different than `flip^rotate`. As `flip` commutes with `rotate^2` this will return `rotate^2`:

```
(rotate^2)^flip
```

!!! Differences:

There is no support for the `Cycle` class

"""
function Permutation(x; kwargs...)
    if typeof(x) <: UnitRange
        x = collect(x)
    end
    _check_permutation_format(x)
    SymPermutation(sympy.combinatorics.permutations.Permutation(x; kwargs...))
end
function Permutation(i, j, xs...; kwargs...)
    Permutation([vcat(i,j,xs...)]; kwargs...)
end
Permutation(;kwargs...) = SymPermutation(sympy.combinatorics.permutations.Permutation(; kwargs...))
export Permutation

# left right
# check commutative
*(p::SymPermutation, q::SymPermutation)::SymPermutation = PyCall.py"$p * $q"
+(p::SymPermutation, i::Integer)::SymPermutation = PyCall.py"$p + $i"
-(p::SymPermutation, i::Integer)::SymPermutation = PyCall.py"$p - $i"
^(p::SymPermutation, i::Integer)::SymPermutation = PyCall.py"$p**$i"
^(p::SymPermutation, i::Sym) = p^N(i)
^(i::Integer, p::SymPermutation) = p(i)   # python has i^p = p(i)
^(i::Sym, p::SymPermutation) = p(i)       # SymPy has i^p = p(i)
^(p::SymPermutation, q::SymPermutation)::SymPermutation  = PyCall.py"$p^$q" # conjugate
inv(p::SymPermutation)::SymPermutation = PyCall.py"$p**(-1)"
/(p::SymPermutation, q::SymPermutation)::SymPermutation = p * inv(q)
import Base: ~
~(p::SymPermutation) = SymPermutation(~(PyCall.PyObject(p)))
Base.inv(p::SymPermutation) = ~p

function +(p::SymPermutation, q::SymPermutation)::SymPermutation
    !is_Identity(commutator(p,q)) && throw(DomainError("p and q do not commute"))
    p * q
end

# evaluation p:i -> j
(p::SymPermutation)(i::Integer) = p.__pyobject__(i)
function (p::SymPermutation)(i::Integer, j::Integer, xs...)
    q = Permutation([vcat(i, j, xs...)])
    p * q
end
(p::SymPermutation)(v::Vector) = PyCall.py"$p($v)"
(p::SymPermutation)(s::String) = p([s[i:i] for i in eachindex(s)])

==(p::SymPermutation, q::SymPermutation) = PyCall.py"$p == $q"
hash(p::SymPermutation) = hash(array_form(p))


Base.length(p::SymPermutation) = p.length
Base.max(p::SymPermutation) = p.max
Base.min(p::SymPermutation) = p.min
rank(p::SymPermutation) = p.rank


## we want perms, not tuples
transpositions(p::SymPermutation) = map(x -> Permutation([x]), object_meth(p, :transpositions))
export transpositions




"""

    PermutationGroup: create Permutation group from group generators

A PermutationGroup is one generated by a collection of permutations.

Some pre-defined groups are built-in:

* `SymmetricgGroup(n)`: S_n or all symmetries of an n-gon
* `CyclicGroup`: the group Z_n
* `DihedralGroup`: Group formed by a flip and rotation
* AlternativeGroup: Subgroup of S_n of even elements
* AbelianGroup: Returns the direct product of cyclic groups with the given orders.


Differences:

* use `collect(generate(G))` in place of `list(G.generate())`

"""
PermutationGroup(args...; kwargs...) = combinatorics.perm_groups.PermutationGroup(args...; kwargs...)
export PermutationGroup

## Algebra therof
import Base: *
*(G1::SymPermutationGroup, G2::SymPermutationGroup)::SymPermutationGroup = PyCall.py"$G1*$G2"

## Indexing into group
Base.getindex(Gp::SymPermutationGroup, i::Int) = PyCall.py"$(Gp.__pyobject__)[$(i-1)]"  # 1-base

elements(gp::SymPermutationGroup) = [a for a in gp.elements]

Base.length(G::SymPermutationGroup) =  PyCall.py"len($(G.__pyobject__))"

"""
   occursin(x, G::SymPermutationGroup)

Does G contain x. (In SymPy, this is `contains.)
"""
Base.occursin(x, G::SymPermutationGroup; kwargs...) = (G.contains(x; kwargs...) == Sym(true)) # was contains
