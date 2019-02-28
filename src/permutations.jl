
#module Permutations

## A permutation of {0, 1, 2, ..., n} -- 0-based
struct SymPermutation <: SymPy.SymbolicObject
x::PyCall.PyObject
end
export SymPermutation



## A permutation of {0, 1, 2, ..., n} -- 0-based
struct SymPermutationGroup <: SymPy.SymbolicObject
  x::PyCall.PyObject
end
export SymPermutationGroup


module Permutations

using SymPy
#import SymPy: degree, is_even, is_odd,
import SymPy: is_primitive
import PyCall
import LinearAlgebra: rank

## raise error if wrong format
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
order(rotate)
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
commutator(flip, rotate^2) == id
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
    SymPy.combinatorics.permutations.Permutation(x; kwargs...)
end
function Permutation(i, j, xs...; kwargs...)
    Permutation([vcat(i,j,xs...)]; kwargs...)
end
Permutation(;kwargs...) = SymPy.combinatorics.permutations.Permutation(; kwargs...)
export Permutation

## name?
## random permutation of {0,1,..., n-1}
randomperm(n) = SymPy.combinatorics.permutations.Permutation.random(n)
export randomperm
# ops
import Base: +, -, *, /, ^, inv
import Base: ==, hash

# left right
# check commutative
*(p::SymPermutation, q::SymPermutation) = PyCall.py"$p * $q"
+(p::SymPermutation, i::Integer) = PyCall.py"$p + $i"
-(p::SymPermutation, i::Integer) = PyCall.py"$p - $i"
^(p::SymPermutation, i::Integer) = PyCall.py"$p**$i"
^(p::SymPermutation, i::Sym) = p^N(i)
^(i::Integer, p::SymPermutation) = p(i)   # python has i^p = p(i)
^(i::Sym, p::SymPermutation) = p(i)       # SymPy has i^p = p(i)
^(p::SymPermutation, q::SymPermutation)  = PyCall.py"$p^$q" # conjugate
inv(p::SymPermutation) = PyCall.py"$p**(-1)"
/(p::SymPermutation, q::SymPermutation) = p * inv(q)

function +(p::SymPermutation, q::SymPermutation)
    !is_Identity(commutator(p,q)) && throw(DomainError("p and q do not commute"))
    p * q
end

# evaluation p:i -> j
(p::SymPermutation)(i::Integer) = p.x(i)
function (p::SymPermutation)(i::Integer, j::Integer, xs...)
    q = Permutation([vcat(i, j, xs...)])
    p * q
end
(p::SymPermutation)(v::Vector) = PyCall.py"$p($v)"
(p::SymPermutation)(s::String) = p([s[i:i] for i in eachindex(s)])

==(p::SymPermutation, q::SymPermutation) = PyCall.py"$p == $q"
hash(p::SymPermutation) = hash(array_form(p))

## Methods

## new functinons in Permutation
permutations_new_functions = (
                              :from_inversion_vector,
                              :from_sequence,
                              :josephus,
                              :rmul,
                              :unrank_lex,
                              :unrank_nonlex,
                              :unrank_trotterjohnson
                           )

for meth in permutations_new_functions
    meth_name = string(meth)
    @eval begin
#         @doc """
# `$($meth_name)`: a SymPy function.
# The SymPy documentation can be found through: http://docs.sympy.org/latest/search.html?q=$($meth_name)
# """ ->
        ($meth)(args...; kwargs...) = getproperty(SymPy.combinatorics.permutations.Permutation,$meth_name)(args...; kwargs...)
    end
    eval(Expr(:export, meth))
end



## Base methods of the object
Base.length(p::SymPermutation) = object_meth(p, :length)
Base.max(p::SymPermutation) = object_meth(p, :max)
Base.min(p::SymPermutation) = object_meth(p, :min)
rank(p::SymPermutation) = object_meth(p, :rank)

## non-Base object methods
for meth in (:ascents,
             :commutator,
             :commutes_with,
             :descents,
             :from_inversion_vector,
             :get_adjacency_distance,
             :get_adjacency_matrix,
             :get_positional_distance,
             :get_precedence_distance,
             :get_precedence_matrix,
             :index,
             :inversion_vector,
             :inversions,
             :list,
    :next_lex,
    :next_nonlex,
    :order,
    :rank_nonlex,
    :rank_trotterjohnson,
    :next_trotterjohnson,
    :parity,
#    :random,                    # hate using this name... randp,perm?
    :runs,
    :signature,
    :support
#             :transpositions
             )

    meth_name = string(meth)
    @eval begin
#         @doc """
# `$($meth_name)`: a SymPy function.
# The SymPy documentation can be found through: http://docs.sympy.org/latest/search.html?q=$($meth_name)
# """ ->
        ($meth)(ex::SymPermutation, args...; kwargs...) = object_meth(ex, $meth_name, args...; kwargs...)
    end
    eval(Expr(:export, meth))
end

## we want perms, not tuples
transpositions(p::SymPermutation) = map(x -> Permutation([x]), object_meth(p, :transpositions))
export transpositions

## properties
## Base methods
Base.size(p::SymPermutation) = p.x.size # property, not method call

## non-base methods
#import SymPy: is_even, is_odd
for prop in (:is_Empty,
             :is_Identity,
             :is_Singleton,
#             :is_even,
#             :is_odd,
             :array_form,
#             :cyclic_form,
#             :full_cyclic_form,
             :cycle_structure,
             :cycles,
             :cardinality,
             )

    prop_name = string(prop)
    @eval begin
#         @doc """
# `$($prop_name)`: a SymPy function.
# The SymPy documentation can be found through: http://docs.sympy.org/latest/search.html?q=$($prop_name)
# """ ->
        ($prop)(ex::SymPermutation) = getproperty(PyCall.PyObject(ex),Symbol($prop_name))
    end
    eval(Expr(:export, prop))
end

_unflatten_cyclic_form(m::Matrix) = [m[i,:] for i in 1:size(m)[1]]
_unflatten_cyclic_form(m) = m

function cyclic_form(p::SymPermutation)
    m = p.cyclic_form
    _unflatten_cyclic_form(m)
end

function full_cyclic_form(p::SymPermutation)
    m = p.full_cyclic_form
    _unflatten_cyclic_form(m)
end

export cyclic_form, full_cyclic_form

## Permutation Groups ################################################

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
PermutationGroup(args...; kwargs...) = SymPy.combinatorics.perm_groups.PermutationGroup(args...; kwargs...)
export PermutationGroup

## Algebra therof
import Base: *
G1::SymPy.SymPermutationGroup *  G2::SymPy.SymPermutationGroup = PyCall.py"$G1*$G2"

## Indexing into group
Base.getindex(Gp::SymPy.SymPermutationGroup, i::Int) = PyCall.py"$(Gp.x)[$(i-1)]"  # 1-base

## Special groups
SymmetricGroup(n::Int) = SymPy.combinatorics.named_groups.SymmetricGroup(n)
CyclicGroup(n::Int) = SymPy.combinatorics.named_groups.CyclicGroup(n)
DihedralGroup(n::Int) = SymPy.combinatorics.named_groups.DihedralGroup(n)
AlternatingGroup(n::Int) = SymPy.combinatorics.named_groups.AlternatingGroup(n)
AbelianGroup(args...) =  SymPy.combinatorics.named_groups.AbelianGroup(args...)

export SymmetricGroup, CyclicGroup, DihedralGroup, AlternatingGroup, AbelianGroup



# special methods
SymPy.elements(gp::SymPy.SymPermutationGroup) = [a for a in gp.elements]

"""
    random_element(gp, ...)

A random group element. Alias to `sympy"random"`.
"""
random_element(gp::SymPy.SymPermutationGroup, args...) = object_meth(gp, :random, args...)

# base_methods
permutation_group_methods_in_base = (#,
                                     )
for meth in permutation_group_methods_in_base
    meth_name = string(meth)
    eval(Expr(:import, :Base, meth))
    @eval begin
#         @doc """
# `$($meth_name)`: a SymPy function.
# The SymPy documentation can be found through: http://docs.sympy.org/latest/search.html?q=$($meth_name)
# """ ->
        ($meth)(ex::SymPermutationGroup, args...; kwargs...) = object_meth(ex, $meth_name, args...; kwargs...)
    end
end


# new methods
permutation_group_methods = (#:baseswap,
                             #:base,
                             :center,
                             :centralizer,
                             :commutator,
                             :coset_factor,
                             :coset_rank,
                             :coset_table,
:coset_transversal,
:coset_unrank,
:derived_series,
:derived_subgroup,
:generate,   # create generator of group, can iterate but no comprehension, map
:generate_dimino,
:generate_schreier_sims,
:is_alt_sym,
:is_normal,
:is_primitive,
:is_subgroup,
:is_transitive,
:lower_central_series,
:make_perm,
:minimal_block,
:normal_closure,
#:orbit,
:orbit_rep,
:orbit_transversal,
:orbits,
:order,
#:pointwise_stabilizer,
:random_pr,
:random_stab,
:schreier_sims,
#:schreier_sims_incremental,
:schreier_sims_random,
:schreier_vector,
:stabilizer,
:subgroup,
:subgroup_search
)

for meth in permutation_group_methods
    meth_name = string(meth)
    @eval begin
#         @doc """
# `$($meth_name)`: a SymPy function.
# The SymPy documentation can be found through: http://docs.sympy.org/latest/search.html?q=$($meth_name)
# """ ->
        ($meth)(ex::SymPermutationGroup, args...; kwargs...) = object_meth(ex, $meth_name, args...; kwargs...)
    end
    eval(Expr(:export, meth))
end

len(G::SymPermutationGroup) = PyCall.py"len($(G.x))"
export len
"""
   occursin(x, G::SymPermutationGroup)

Does G contain x. (In SymPy, this is `contains.)
"""
Base.occursin(x, G::SymPermutationGroup; kwargs...) = (G.contains(x; kwargs...) == Sym(true)) # was contains

## These need PyVector in the call
orbit(G::SymPy.SymPermutationGroup, alpha::Number, args...; kwargs...) = object_meth(G, :orbit, alpha, args...; kwargs...)
orbit(G::SymPy.SymPermutationGroup, alpha::Union{Vector, Tuple}, args...; kwargs...) = object_meth(G, :orbit, PyCall.PyVector(alpha), args...; kwargs...)
export orbit

pointwise_stabilizer(G::SymPy.SymPermutationGroup, points, args...; kwargs...) = object_meth(G, :pointwise_stabilizer, PyCall.PyVector(points), args...; kwargs...)
export pointwise_stabilizer

schreier_sims_incremental(G::SymPy.SymPermutationGroup, args...; base=[], kwargs...) = object_meth(G, :schreier_sims_incremental, args..., base=PyCall.PyVector(base), kwargs...)
export schreier_sims_incremental

baseswap(G::SymPy.SymPermutationGroup, base, strong_gens, pos, args...; kwargs...) = object_meth(G, :baseswap, PyCall.PyVector(base), strong_gens, pos, args...; kwargs...)
export baseswap

# base methods as properties


# non-base properties
import SymPy: degree
permutation_group_properties = (:basic_orbits,
                                :basic_stabilizers,
                                :basic_transversals,
                                :degree,
                                :is_group,
                                :is_abelian,
                                :is_nilpotent,
                                :is_solvable,
                                :is_trivial,
:generators,
:max_div,
                                :transitivity_degree

                                )
for prop in permutation_group_properties
    prop_name = string(prop)
    @eval begin
#         @doc """
# `$($prop_name)`: a SymPy function.
# The SymPy documentation can be found through: http://docs.sympy.org/latest/search.html?q=$($prop_name)
# """ ->
        ($prop)(ex::SymPermutationGroup) = getproperty(PyCall.PyObject(ex),Symbol($prop_name))
    end
    eval(Expr(:export, prop))
end


base(ex::SymPermutationGroup) = ex.base
export base



strong_gens(D::SymPermutationGroup) = D.strong_gens
export strong_gens


end
