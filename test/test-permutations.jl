# Permutations

## Tests from the examples in
## * http://docs.sympy.org/latest/modules/combinatorics/permutations.html and
## * http://docs.sympy.org/latest/modules/combinatorics/perm_groups.html

## Syntax modifications needed to convert Python code to SymPy.jl code
## p.size(a,b,c) -> size(p,a,b,c)
## p.property -> p |> property or property(p)
## A few cases need a function to be passed in and notation (pymethod) for this needs to be ironed out.

using SymPy
using SymPy.Permutations
using PyCall
using Base.Test
Base.range(i::Int64) = 0:(i-1)

@testset "Permutations" begin



# Array Notation And 2-line Form
p = Permutation([0, 2, 1]); p
# Permutation([0, 2, 1])

# Given i in range(size(p)), the permutation maps i to i^p
@test [i^p for i in range(size(p))] == [0,2,1]
# [0, 2, 1]

## The composite of two permutations p*q means first apply p, then q, so i^(p*q) = (i^p)^q which is i^p^q according to Python precedence rules:

q = Permutation([2, 1, 0])
@test [(i^p)^q for i in range(3)] == [2,0,1]  ## JULIA AND PYTHON DIFFER
# [2, 0, 1]
@test [i^(p*q) for i in range(3)] == [2,0,1]
# [2, 0, 1]
# One can use also the notation p(i) = i^p, but then the composition rule is (p*q)(i) = q(p(i)), not p(q(i)):

@test [(p*q)(i) for i in range(size(p))] == [2,0,1]
# [2, 0, 1]
@test [q(p(i)) for i in range(size(p))] == [2,0,1]
# [2, 0, 1]
@test [p(q(i)) for i in range(size(p))] == [1,2,0]
# [1, 2, 0]


# Disjoint Cycle Notation

# In disjoint cycle notation, only the elements that have shifted are indicated. In the above case, the 2 and 1 switched places. This can be entered in two ways:

@test Permutation(1, 2) == Permutation([[1, 2]]) == p
# True
# Only the relative ordering of elements in a cycle matter:


@test Permutation(1,2,3) == Permutation(2,3,1) == Permutation(3,1,2)
# True
# The disjoint cycle notation is convenient when representing permutations that have several cycles in them:


@test Permutation(1, 2)(3, 5) == Permutation([[1, 2], [3, 5]])
# True
# It also provides some economy in entry when computing products of permutations that are written in disjoint cycle notation:


wtf = Permutation(1, 2)(1, 3)(2, 3)
# Permutation([0, 3, 2, 1])
@test wtf == Permutation([[1, 2]])*Permutation([[1, 3]])*Permutation([[2, 3]])  ## ans = _
# True
# Caution: when the cycles have common elements between them then the order in which the permutations are applied matters. The convention is that the permutations are applied from right to left. In the following, the transposition of elements 2 and 3 is followed by the transposition of elements 1 and 2:


@test Permutation(1, 2)(2, 3) == Permutation([(1, 2), (2, 3)])
# True
@test list(Permutation(1, 2)(2, 3)) == [0,3,1,2]
# [0, 3, 1, 2]
# If the first and second elements had been swapped first, followed by the swapping of the second and third, the result would have been [0, 2, 3, 1]. If, for some reason, you want to apply the cycles in the order they are entered, you can simply reverse the order of cycles:


@test (Permutation([(1, 2), (2, 3)] |> reverse) |> list) == [0,2,3,1]
# [0, 2, 3, 1]
# Entering a singleton in a permutation is a way to indicate the size of the permutation. The size keyword can also be used.

# Array-form entry:


Permutation([[1, 2], [9]])
# Permutation([0, 2, 1], size=10)
Permutation([[1, 2]], size=10)
# Permutation([0, 2, 1], size=10)
# Cyclic-form entry:


p = Permutation(1, 2, size=10)
# Permutation([0, 2, 1], size=10)
Permutation(9)(1, 2) == p
# Permutation([0, 2, 1], size=10)
# Caution: no singleton containing an element larger than the largest in any previous cycle can be entered. This is an important difference in how Permutation and Cycle handle the __call__ syntax. A singleton argument at the start of a Permutation performs instantiation of the Permutation and is permitted:


Permutation(5)
# Permutation([], size=6)
# A singleton entered after instantiation is a call to the permutation – a function call – and if the argument is out of range it will trigger an error. For this reason, it is better to start the cycle with the singleton:

# The following fails because there is is no element 3:


## This throws as expected Permutation(1, 2)(3)
# Traceback (most recent call last):
# ...
# IndexError: list index out of range
# This is ok: only the call to an out of range singleton is prohibited; otherwise the permutation autosizes:


Permutation(3)(1, 2)
# Permutation([0, 2, 1, 3])
@test Permutation(1, 2)(3, 4) == Permutation(3, 4)(1, 2)
# True
# Equality Testing

# The array forms must be the same in order for permutations to be equal:


@test !(Permutation([1, 0, 2, 3]) == Permutation([1, 0]))
# False
# Identity Permutation

# The identity permutation is a permutation in which no element is out of place. It can be entered in a variety of ways. All the following create an identity permutation of size 4:


I = Permutation([0, 1, 2, 3])
@test all(p == I for p in [Permutation(3), Permutation(range(4)), Permutation([], size=4), Permutation(size=4)])
# ... Permutation(3),
# ... Permutation(range(4)),
# ... Permutation([], size=4),
# ... Permutation(size=4)])
# True
# Watch out for entering the range inside a set of brackets (which is cycle notation):


@test I != Permutation([range(4)])
# False
# Permutation Printing

# There are a few things to note about how Permutations are printed.

# 1) If you prefer one form (array or cycle) over another, you can set that with the print_cyclic flag.


p = Permutation(1, 2)(4, 5)(3, 4)
# Permutation([0, 2, 1, 4, 5, 3])

### p
# (1 2)(3 4 5)
# 2) Regardless of the setting, a list of elements in the array for cyclic form can be obtained and either of those can be copied and supplied as the argument to Permutation:


@test array_form(p) == [0, 2, 1, 4, 5, 3]
# [0, 2, 1, 4, 5, 3]
@test cyclic_form(p) == [[1, 2], [3, 4, 5]]
# [[1, 2], [3, 4, 5]]
@test Permutation(cyclic_form(p)) == p
# True
# 3) Printing is economical in that as little as possible is printed while retaining all information about the size of the permutation:


Permutation([1, 0, 2, 3])
# Permutation([1, 0, 2, 3])
Permutation([1, 0, 2, 3], size=20)
# Permutation([1, 0], size=20)
Permutation([1, 0, 2, 4, 3, 5, 6], size=20)
# Permutation([1, 0, 2, 4, 3], size=20)

p = Permutation([1, 0, 2, 3])
p
# (3)(0 1)
# The 2 was not printed but it is still there as can be seen with the array_form and size methods:


@test array_form(p) == [1, 0, 2, 3]
# [1, 0, 2, 3]
@test size(p) == 4
# 4
# Short Introduction To Other Methods

# The permutation can act as a bijective function, telling what element is located at a given position


q = Permutation([5, 2, 3, 4, 1, 0])
@test array_form(q)[2] == 2 # the hard way -- indexing is 1-base
# 2
@test q(1) == 2 # the easy way -- function call 0-based
# 2
@test [i => q(i) for i in range(size(q))] == [0 => 5, 1 => 2, 2 => 3, 3 => 4, 4 => 1, 5 => 0]
# {0: 5, 1: 2, 2: 3, 3: 4, 4: 1, 5: 0}
# The full cyclic form (including singletons) can be obtained:


@test full_cyclic_form(p) == [[0, 1], [2], [3]]
# [[0, 1], [2], [3]]
# Any permutation can be factored into transpositions of pairs of elements:


as = [Permutation(1, 2), Permutation(3, 5), Permutation(3, 4)]
@test transpositions(Permutation([[1, 2], [3, 4, 5]])) == as
# [(1, 2), (3, 5), (3, 4)]
@test *([Permutation(ai, size=6) for ai in as]...) == Permutation(1,2)(3,5,4)
@test rmul([Permutation(ai, size=6) for ai in as]...) == Permutation([[1, 2], [3, 4, 5]])
# [[1, 2], [3, 4, 5]]


# The number of permutations on a set of n elements is given by n! and is called the cardinality.
@test size(p) == 4
# 4
@test cardinality(p) == 24 
# 24

# A given permutation has a rank among all the possible permutations of the same elements, but what that rank is depends on how the permutations are enumerated. (There are a number of different methods of doing so.) The lexicographic rank is given by the rank method and this rank is used to increment a permutation with addition/subtraction:

@test rank(p) == 6
# 6
@test p + 1 == Permutation([1, 0, 3, 2])
# Permutation([1, 0, 3, 2])
tmp = next_lex(p)
@test tmp == Permutation([1, 0, 3, 2])
# Permutation([1, 0, 3, 2])
@test rank(tmp) == 7
# 7
@test unrank_lex(size(p), rank=7) == Permutation([1, 0, 3, 2])
# Permutation([1, 0, 3, 2])
# The product of two permutations p and q is defined as their composition as functions, (p*q)(i) = q(p(i)) [R33].


p = Permutation([1, 0, 2, 3])
q = Permutation([2, 3, 1, 0])
@test list(q*p) == [2, 3, 0, 1]
# [2, 3, 0, 1]
@test list(p*q) == [3,2,1,0]
# [3, 2, 1, 0]
@test [q(p(i)) for i in range(size(p))] == [3,2,1,0]
# [3, 2, 1, 0]
# The permutation can be ‘applied’ to any list-like object, not only Permutations:


@test p(["zero", "one", "four", "two"]) ==  ["one", "zero", "four", "two"]
#  ['one', 'zero', 'four', 'two']
@test p("zo42") == ["o", "z", "4", "2"]
# ['o', 'z', '4', '2']

## -----

# If you have a list of arbitrary elements, the corresponding permutation can be found with the from_sequence method:
@test from_sequence("SymPy") ==  Permutation([1, 3, 2, 0, 4])
# Permutation([1, 3, 2, 0, 4])

## -----

# array_form
# Return a copy of the attribute _array_form Examples ========

p = Permutation([[2, 0], [3, 1]])
@test array_form(p) == [2, 3, 0, 1]
# [2, 3, 0, 1]
Permutation([[2, 0, 3, 1]]) |> array_form
# [3, 2, 0, 1]
Permutation([2, 0, 3, 1]) |> array_form
# [2, 0, 3, 1]
Permutation([[1, 2], [4, 5]]) |> array_form
# [0, 2, 1, 3, 5, 4]


## -----

# ascents()[source]
# Returns the positions of ascents in a permutation, ie, the location where p[i] < p[i+1]

# See also descents, inversions, min, max
# Examples

p = Permutation([4, 0, 1, 3, 2])
@test ascents(p) == [1,2]
# [1, 2]


## -----

# atoms()[source]
# Returns all the elements of a permutation

# Examples


#from sympy.combinatorics import Permutation
@test collect(atoms(Permutation([0, 1, 2, 3, 4, 5]))) == [0,1,2,3,4,5]
# {0, 1, 2, 3, 4, 5}
@test collect(atoms(Permutation([[0, 1], [2, 3], [4, 5]]))) == [0,1,2,3,4,5]
# {0, 1, 2, 3, 4, 5}
# cardinality
# Returns the number of all possible permutations.

# See also length, order, rank, size
# Examples


#from sympy.combinatorics.permutations import Permutation
p = Permutation([0, 1, 2, 3])
@test cardinality(p) == 24
# 24

## -----

# commutator(x)[source]
# Return the commutator of self and x: ~x*~self*x*self

# If f and g are part of a group, G, then the commutator of f and g is the group identity iff f and g commute, i.e. fg == gf.

# References

# http://en.wikipedia.org/wiki/Commutator

# Examples


#from sympy.combinatorics.permutations import Permutation

p = Permutation([0, 2, 3, 1])
x = Permutation([2, 0, 3, 1])
c = commutator(p, x);
@test c == Permutation([2, 1, 3, 0])
# Permutation([2, 1, 3, 0])
@test c == inv(x)*inv(p)*x*p
# True

Id = Permutation(3)
p = [(Id + i) for i in range(6)]
for i in eachindex(p)
    for j in eachindex(p)  
    c = commutator(p[i], p[j])
    if p[i]*p[j] == p[j]*p[i]
        @assert c == I
    else
        @assert c != I
    end
    end
end



## -----


# commutes_with(other)[source]
# Checks if the elements are commuting.

# Examples


#from sympy.combinatorics.permutations import Permutation
a = Permutation([1, 4, 3, 0, 2, 5])
b = Permutation([0, 1, 2, 3, 4, 5])
@test commutes_with(a, b)
# True
b = Permutation([2, 3, 5, 4, 1, 0])
@test !commutes_with(a, b)
# False
# cycle_structure
# Return the cycle structure of the permutation as a dictionary indicating the multiplicity of each cycle length.

# Examples


#from sympy.combinatorics import Permutation

@test cycle_structure(Permutation(3)) ==  Dict{Any,Any}(1=>4)
# {1: 4} 
@test cycle_structure(Permutation(0, 4, 3)(1, 2)(5, 6)) == Dict{Any,Any}(2=>2, 3=>1)
# {2: 2, 3: 1}
# cycles
# Returns the number of cycles contained in the permutation (including singletons).

# See also sympy.functions.combinatorial.numbers.stirling
# Examples


#from sympy.combinatorics import Permutation
@test cycles(Permutation([0, 1, 2])) == 3
# 3
@test full_cyclic_form(Permutation([0, 1, 2])) ==  [[0], [1], [2]]
# [[0], [1], [2]]
@test cycles(Permutation(0, 1)(2, 3)) == 2
# 2

# cyclic_form
# This is used to convert to the cyclic notation from the canonical notation. Singletons are omitted.

# See also array_form, full_cyclic_form
# Examples


#from sympy.combinatorics.permutations import Permutation
p = Permutation([0, 3, 1, 2])
@test cyclic_form(p) ==[[1, 3, 2]] 
# [[1, 3, 2]]
@test cyclic_form(Permutation([1, 0, 2, 4, 3, 5])) == [[0, 1], [3, 4]] 
# [[0, 1], [3, 4]]





## -----

# descents()[source]
# Returns the positions of descents in a permutation, ie, the location where p[i] > p[i+1]

# See also ascents, inversions, min, max
# Examples


#from sympy.combinatorics.permutations import Permutation
p = Permutation([4, 0, 1, 3, 2])
@test descents(p) == [0, 3]
# [0, 3]


## -----

# classmethod from_inversion_vector(inversion)[source]
# Calculates the permutation from the inversion vector.

# Examples


#from sympy.combinatorics.permutations import Permutation
@test from_inversion_vector([3, 2, 1, 0, 0]) == Permutation([3, 2, 1, 0, 4, 5])
# Permutation([3, 2, 1, 0, 4, 5])




## -----

# classmethod from_sequence(i, key=None)[source]
# Return the permutation needed to obtain i from the sorted elements of i. If custom sorting is desired, a key can be given.

# Examples


#from sympy.combinatorics import Permutation

@test from_sequence("SymPy") == Permutation(4)(0,1,3)
# (4)(0 1 3)

#_(sorted("SymPy"))
# ['S', 'y', 'm', 'P', 'y']

### XXX Passing in a method should be done via `pymethod`, though
## 
#Permutation.from_sequence('SymPy', key=lambda x: x.lower()) XXX
# (4)(0 2)(1 3)


# full_cyclic_form
# Return permutation in cyclic form including singletons.

# Examples


#from sympy.combinatorics.permutations import Permutation
@test full_cyclic_form(Permutation([0, 2, 1])) == [[0], [1,2]]
# [[0], [1, 2]]



## -----

# get_adjacency_distance(other)[source]
# Computes the adjacency distance between two permutations.

# This metric counts the number of times a pair i,j of jobs is adjacent in both p and p’. If n_adj is this quantity then the adjacency distance is n - n_adj - 1 [1]

# [1] Reeves, Colin R. Landscapes, Operators and Heuristic search, Annals of Operational Research, 86, pp 473-490. (1999)

# See also get_precedence_matrix, get_precedence_distance, get_adjacency_matrix
# Examples


#from sympy.combinatorics.permutations import Permutation
p = Permutation([0, 3, 1, 2, 4])
q = josephus(4, 5, 2)
@test get_adjacency_distance(p, q) == 3
# 3
r = Permutation([0, 2, 1, 4, 3])
@test get_adjacency_distance(p, r) == 4
# 4


## -----

# get_adjacency_matrix()[source]
# Computes the adjacency matrix of a permutation.

# If job i is adjacent to job j in a permutation p then we set m[i, j] = 1 where m is the adjacency matrix of p.

# See also get_precedence_matrix, get_precedence_distance, get_adjacency_distance
# Examples


#from sympy.combinatorics.permutations import Permutation
p = josephus(3, 6, 1)
@test get_adjacency_matrix(p) == Sym[0  0  0  0  0  0; 
0  0  0  0  1  0; 
0  0  0  0  0  1; 
0  1  0  0  0  0; 
1  0  0  0  0  0; 
0  0  0  1  0  0]
# Matrix([
# [0, 0, 0, 0, 0, 0],
# [0, 0, 0, 0, 1, 0],
# [0, 0, 0, 0, 0, 1],
# [0, 1, 0, 0, 0, 0],
# [1, 0, 0, 0, 0, 0],
# [0, 0, 0, 1, 0, 0]])
q = Permutation([0, 1, 2, 3])
@test get_adjacency_matrix(q) == Sym[0  1  0  0;
0  0  1  0;
0  0  0  1;
0  0  0  0]
# Matrix([
# [0, 1, 0, 0],
# [0, 0, 1, 0],
# [0, 0, 0, 1],
# [0, 0, 0, 0]])


## -----

# get_positional_distance(other)[source]
# Computes the positional distance between two permutations.

# See also get_precedence_distance, get_adjacency_distance
# Examples


#from sympy.combinatorics.permutations import Permutation
p = Permutation([0, 3, 1, 2, 4])
q = josephus(4, 5, 2)
r = Permutation([3, 1, 4, 0, 2])
@test get_positional_distance(p, q) == 12
# 12
@test get_positional_distance(p, r) == 12
# 12


## -----

# get_precedence_distance(other)[source]
# Computes the precedence distance between two permutations.

# Suppose p and p’ represent n jobs. The precedence metric counts the number of times a job j is preceded by job i in both p and p’. This metric is commutative.

# See also get_precedence_matrix, get_adjacency_matrix, get_adjacency_distance
# Examples


#from sympy.combinatorics.permutations import Permutation
p = Permutation([2, 0, 4, 3, 1])
q = Permutation([3, 1, 2, 4, 0])
@test get_precedence_distance(p, q) == 7
# 7
@test get_precedence_distance(q, p) == 7
# 7


## -----

# get_precedence_matrix()[source]
# Gets the precedence matrix. This is used for computing the distance between two permutations.

# See also get_precedence_distance, get_adjacency_matrix, get_adjacency_distance
# Examples


#from sympy.combinatorics.permutations import Permutation
p = josephus(3, 6, 1)
@test p  ==  Permutation([2, 5, 3, 1, 4, 0])
# Permutation([2, 5, 3, 1, 4, 0])
@test get_precedence_matrix(p) == Sym[
0  0  0  0  0  0;
1  0  0  0  1  0;
1  1  0  1  1  1;
1  1  0  0  1  0;
1  0  0  0  0  0;
1  1  0  1  1  0]
# Matrix([
# [0, 0, 0, 0, 0, 0],
# [1, 0, 0, 0, 1, 0],
# [1, 1, 0, 1, 1, 1],
# [1, 1, 0, 0, 1, 0],
# [1, 0, 0, 0, 0, 0],
# [1, 1, 0, 1, 1, 0]])


## -----

# index()[source]
# Returns the index of a permutation.

# The index of a permutation is the sum of all subscripts j such that p[j] is greater than p[j+1].

# Examples


#from sympy.combinatorics.permutations import Permutation
p = Permutation([3, 0, 2, 1, 4])
@test index(p) == 2
# 2


## -----

# inversion_vector()[source]
# Return the inversion vector of the permutation.

# The inversion vector consists of elements whose value indicates the number of elements in the permutation that are lesser than it and lie on its right hand side.

# The inversion vector is the same as the Lehmer encoding of a permutation.

# See also from_inversion_vector
# Examples


#from sympy.combinatorics.permutations import Permutation
p = Permutation([4, 8, 0, 7, 1, 5, 3, 6, 2])
@test inversion_vector(p) == [4, 7, 0, 5, 0, 2, 1, 1]
# [4, 7, 0, 5, 0, 2, 1, 1]
p = Permutation([3, 2, 1, 0])

@test inversion_vector(p) == [3,2,1]
# [3, 2, 1]
# The inversion vector increases lexicographically with the rank of the permutation, the -ith element cycling through 0..i.


p = Permutation(2)
while p != nothing
    p = next_lex(p)
end
#while p:
# ...     print('%s %s %s' % (p, p.inversion_vector(), p.rank()))
# ...     p = p.next_lex()
# ...
# Permutation([0, 1, 2]) [0, 0] 0
# Permutation([0, 2, 1]) [0, 1] 1
# Permutation([1, 0, 2]) [1, 0] 2
# Permutation([1, 2, 0]) [1, 1] 3
# Permutation([2, 0, 1]) [2, 0] 4
# Permutation([2, 1, 0]) [2, 1] 5


## -----

# inversions()[source]
# Computes the number of inversions of a permutation.

# An inversion is where i > j but p[i] < p[j].

# For small length of p, it iterates over all i and j values and calculates the number of inversions. For large length of p, it uses a variation of merge sort to calculate the number of inversions.

# See also descents, ascents, min, max
# References

# [1] http://www.cp.eng.chula.ac.th/~piak/teaching/algo/algo2008/count-inv.htm

# Examples


#from sympy.combinatorics.permutations import Permutation
p = Permutation([0, 1, 2, 3, 4, 5])
@test inversions(p) == 0
# 0
@test Permutation([3, 2, 1, 0]) |> inversions == 6
# 6
# is_Empty
# Checks to see if the permutation is a set with zero elements

# See also is_Singleton
# Examples


#from sympy.combinatorics import Permutation
@test Permutation([]) |> is_Empty
# True
@test !(Permutation([0]) |> is_Empty)
# False
# is_Identity
# Returns True if the Permutation is an identity permutation.

# See also order
# Examples


#from sympy.combinatorics.permutations import Permutation
p = Permutation([])
@test p |> is_Identity
# True
p = Permutation([[0], [1], [2]])
@test p |> is_Identity
# True
p = Permutation([0, 1, 2])
@test p |> is_Identity
# True
p = Permutation([0, 2, 1])
@test !(p |> is_Identity)
# False
# is_Singleton
# Checks to see if the permutation contains only one number and is thus the only possible permutation of this set of numbers

# See also is_Empty
# Examples


#from sympy.combinatorics import Permutation
@test Permutation([0]) |> is_Singleton
# True
@test !(Permutation([0, 1]) |> is_Singleton)
# False
# is_even
# Checks if a permutation is even.

# See also is_odd
# Examples


#from sympy.combinatorics.permutations import Permutation
p = Permutation([0, 1, 2, 3])
@test p |> is_even
# True
p = Permutation([3, 2, 1, 0])
@test p |> is_even
# True
# is_odd
# Checks if a permutation is odd.

# See also is_even
# Examples


#from sympy.combinatorics.permutations import Permutation
p = Permutation([0, 1, 2, 3])
@test !(p |> is_odd)
# False
p = Permutation([3, 2, 0, 1])
@test p |>is_odd
# True


## -----

# classmethod josephus(m, n, s=1)[source]
# Return as a permutation the shuffling of range(n) using the Josephus scheme in which every m-th item is selected until all have been chosen. The returned permutation has elements listed by the order in which they were selected.

# The parameter s stops the selection process when there are s items remaining and these are selected by continuing the selection, counting by 1 rather than by m.

# Consider selecting every 3rd item from 6 until only 2 remain:

# choices    chosen
# ========   ======
#   012345
#   01 345   2
#   01 34    25
#   01  4    253
#   0   4    2531
#   0        25314
#            253140
# References

# http://en.wikipedia.org/wiki/Flavius_Josephus
# http://en.wikipedia.org/wiki/Josephus_problem
# http://www.wou.edu/~burtonl/josephus.html
# Examples


#from sympy.combinatorics import Permutation
@test josephus(3, 6, 2) |> array_form == [2, 5, 3, 1, 4, 0]
# [2, 5, 3, 1, 4, 0]


## -----

# length()[source]
# Returns the number of integers moved by a permutation.

# See also min, max, support, cardinality, order, rank, size
# Examples


#from sympy.combinatorics import Permutation
@test Permutation([0, 3, 2, 1]) |> length == 2
# 2
@test Permutation([[0, 1], [2, 3]]) |> length == 4
# 4


## -----

# list(size=None)[source]
# Return the permutation as an explicit list, possibly trimming unmoved elements if size is less than the maximum element in the permutation; if this is desired, setting size=-1 will guarantee such trimming.

# Examples


#from sympy.combinatorics.permutations import Permutation
p = Permutation(2, 3)(4, 5)
@test list(p) == [0, 1, 3, 2, 5, 4]
# [0, 1, 3, 2, 5, 4]
@test list(p, size=10) == [0, 1, 3, 2, 5, 4, 6, 7, 8, 9]
# [0, 1, 3, 2, 5, 4, 6, 7, 8, 9]
# Passing a length too small will trim trailing, unchanged elements in the permutation:


@test Permutation(2, 4)(1, 2, 4) |> x -> list(x, -1) == [0, 2, 1]
# [0, 2, 1]
@test Permutation(3) |> x -> list(x, -1) == Int64[]
# []


## -----

# max()[source]
# The maximum element moved by the permutation.

# See also min, descents, ascents, inversions
# Examples


#from sympy.combinatorics.permutations import Permutation
p = Permutation([1, 0, 2, 3, 4])
@test max(p) == 1
# 1


## -----

# min()[source]
# The minimum element moved by the permutation.

# See also max, descents, ascents, inversions
# Examples


#from sympy.combinatorics.permutations import Permutation
p = Permutation([0, 1, 4, 3, 2])
@test min(p) == 2
# 2


## -----

# mul_inv(other)[source]
# other*~self, self and other have _array_form



## -----

# next_lex()[source]
# Returns the next permutation in lexicographical order. If self is the last permutation in lexicographical order it returns None. See [4] section 2.4.

# See also rank, unrank_lex
# Examples


#from sympy.combinatorics.permutations import Permutation
p = Permutation([2, 3, 1, 0])
p = Permutation([2, 3, 1, 0])
@test rank(p) == 17
# 17
p = next_lex(p)
@test rank(p) == 18
# 18


## -----

# next_nonlex()[source]
# Returns the next permutation in nonlex order [3]. If self is the last permutation in this order it returns None.

# See also rank_nonlex, unrank_nonlex
# Examples


#from sympy.combinatorics.permutations import Permutation
p = Permutation([2, 0, 3, 1])
@test rank_nonlex(p) == 5
# 5
p = next_nonlex(p); p
# Permutation([3, 0, 1, 2])
@test rank_nonlex(p) == 6
# 6


## -----

# next_trotterjohnson()[source]
# Returns the next permutation in Trotter-Johnson order. If self is the last permutation it returns None. See [4] section 2.4. If it is desired to generate all such permutations, they can be generated in order more quickly with the generate_bell function.

# See also rank_trotterjohnson, unrank_trotterjohnson, sympy.utilities.iterables.generate_bell
# Examples


#from sympy.combinatorics.permutations import Permutation
p = Permutation([3, 0, 2, 1])
@test rank_trotterjohnson(p) == 4
# 4
p = next_trotterjohnson(p); p
# Permutation([0, 3, 2, 1])
@test rank_trotterjohnson(p) == 5
# 5


## -----

# order()[source]
# Computes the order of a permutation.

# When the permutation is raised to the power of its order it equals the identity permutation.

# See also identity, cardinality, length, rank, size
# Examples


#from sympy.combinatorics.permutations import Permutation
p = Permutation([3, 1, 5, 2, 4, 0])
@test order(p) == 4
# 4
@test (p^(order(p)))  == Permutation([], size=6)
# Permutation([], size=6)


## -----

# parity()[source]
# Computes the parity of a permutation.

# The parity of a permutation reflects the parity of the number of inversions in the permutation, i.e., the number of pairs of x and y such that x > y but p[x] < p[y].

# See also _af_parity
# Examples


#from sympy.combinatorics.permutations import Permutation
p = Permutation([0, 1, 2, 3])
@test parity(p) == 0
# 0
p = Permutation([3, 2, 0, 1])
@test parity(p) == 1
# 1


## -----

# classmethod random(n)[source]
# Generates a random permutation of length n.

# Uses the underlying Python pseudo-random number generator.

# Examples


#from sympy.combinatorics.permutations import Permutation
@test randomperm(2) in (Permutation([1, 0]), Permutation([0, 1]))
# True


## -----

# rank()[source]
# Returns the lexicographic rank of the permutation.

# See also next_lex, unrank_lex, cardinality, length, order, size
# Examples


#from sympy.combinatorics.permutations import Permutation
p = Permutation([0, 1, 2, 3])
@test rank(p) == 0
# 0
p = Permutation([3, 2, 1, 0])
@test rank(p) == 23
# 23


## -----

# rank_nonlex(inv_perm=None)[source]
# This is a linear time ranking algorithm that does not enforce lexicographic order [3].

# See also next_nonlex, unrank_nonlex
# Examples


#from sympy.combinatorics.permutations import Permutation
p = Permutation([0, 1, 2, 3])
@test rank_nonlex(p) == 23
# 23


## -----

# rank_trotterjohnson()[source]
# Returns the Trotter Johnson rank, which we get from the minimal change algorithm. See [4] section 2.4.

# See also unrank_trotterjohnson, next_trotterjohnson
# Examples


#from sympy.combinatorics.permutations import Permutation
p = Permutation([0, 1, 2, 3])
@test rank_trotterjohnson(p) == 0
# 0
p = Permutation([0, 2, 1, 3])
@test rank_trotterjohnson(p) == 7
# 7


## -----

# static rmul(*args)[source]
# Return product of Permutations [a, b, c, …] as the Permutation whose ith value is a(b(c(i))).

# a, b, c, … can be Permutation objects or tuples.

# Notes

# All items in the sequence will be parsed by Permutation as necessary as long as the first item is a Permutation:


a, b = Permutation(1,2,0), Permutation(0,2,1)
## XXX--->>> Permutation.rmul(a, [0, 2, 1]) == Permutation.rmul(a, b) # this fails, [0,2,1] not promoted
@test Permutation(rmul(a, b)...) == rmul(a, b)  
# True
# The reverse order of arguments will raise a TypeError.

# Examples


#from sympy.combinatorics.permutations import _af_rmul, Permutation


## rmul???? XXX--->>> WTF <<<---XXX
a, b = Permutation([1, 0, 2]), Permutation([0, 2, 1])
@test a*b == Permutation(0, 2, 1)
@test list(rmul(a, b)) == [1,2,0]
## [1, 2, 0]
@test [a(b(i)) for i in 0:2] == [1, 2, 0]
# [1, 2, 0]
# This handles the operands in reverse order compared to the * operator:


a = Permutation(a); b = Permutation(b)
@test list(a*b) == [2, 0, 1]
# [2, 0, 1]
@test [b(a(i)) for i in range(3)] == [2, 0, 1]
# [2, 0, 1]


## -----

# classmethod rmul_with_af(*args)[source]
# same as rmul, but the elements of args are Permutation objects which have _array_form



## -----

# runs()[source]
# Returns the runs of a permutation.

# An ascending sequence in a permutation is called a run [5].

# Examples


#from sympy.combinatorics.permutations import Permutation
p = Permutation([2, 5, 7, 3, 6, 0, 1, 4, 8])
@test runs(p) == [[2, 5, 7], [3, 6], [0, 1, 4, 8]]
# [[2, 5, 7], [3, 6], [0, 1, 4, 8]]
q = Permutation([1,3,2,0])
@test runs(q) == [[1, 3], [2], [0]]
# [[1, 3], [2], [0]]


## -----

# signature()[source]
# Gives the signature of the permutation needed to place the elements of the permutation in canonical order.

# The signature is calculated as (-1)^<number of inversions>

# See also inversions
# Examples


#from sympy.combinatorics.permutations import Permutation
p = Permutation([0, 1, 2])
@test inversions(p) == 0
# 0
@test signature(p) == 1
# 1
q = Permutation([0,2,1])
@test inversions(q) == 1
# 1
@test signature(q) == -1
# -1
# size
# Returns the number of elements in the permutation.

# See also cardinality, length, order, rank
# Examples


#from sympy.combinatorics import Permutation
@test Permutation([[3, 2], [0, 1]]) |> size == 4
# 4


## -----

# support()[source]
# Return the elements in permutation, P, for which P[i] != i.

# Examples


#from sympy.combinatorics import Permutation
p = Permutation([[3, 2], [0, 1], [4]])
@test p |> array_form == [1, 0, 3, 2, 4]
# [1, 0, 3, 2, 4]
@test support(p) == [0, 1, 2, 3]
# [0, 1, 2, 3]


## -----

# transpositions()[source]
# Return the permutation decomposed into a list of transpositions.

# It is always possible to express a permutation as the product of transpositions, see [1]

# References

# http://en.wikipedia.org/wiki/Transposition_%28mathematics%29#Properties
# Examples


#from sympy.combinatorics.permutations import Permutation
p = Permutation([[1, 2, 3], [0, 4, 5, 6, 7]])
t = transpositions(p)
@test t == [Permutation(x...) for x in  [(0, 7), (0, 6), (0, 5), (0, 4), (1, 3), (1, 2)]]
# [(0, 7), (0, 6), (0, 5), (0, 4), (1, 3), (1, 2)]
#print(''.join(str(c) for c in t))
# (0, 7)(0, 6)(0, 5)(0, 4)(1, 3)(1, 2)
@test rmul([Permutation(ti, size=size(p)) for ti in t]...) == p
# True


## -----

# classmethod unrank_lex(size, rank)[source]
# Lexicographic permutation unranking.


# See also rank, next_lex
# Examples


#from sympy.combinatorics.permutations import Permutation
@test unrank_nonlex(4, 5) == Permutation([2, 0, 3, 1])
# Permutation([2, 0, 3, 1])
@test unrank_nonlex(4, -1) == Permutation([0, 1, 2, 3])
# Permutation([0, 1, 2, 3])


## -----

# classmethod unrank_trotterjohnson(size, rank)[source]
# Trotter Johnson permutation unranking. See [4] section 2.4.

# See also rank_trotterjohnson, next_trotterjohnson
# Examples


#from sympy.combinatorics.permutations import Permutation
@test unrank_trotterjohnson(5, 10) == Permutation([0, 3, 1, 2, 4])
# Permutation([0, 3, 1, 2, 4])


## ----- NO CYCLE SUPPORT
## 8< .... cut .... >8
end

@testset "PermutationGroups" begin



    ## -----

    # class sympy.combinatorics.perm_groups.PermutationGroup[source]
# The class defining a Permutation group.
# The permutations corresponding to motion of the front, right and bottom face of a 2x2 Rubik’s cube are defined:


F = Permutation(2, 19, 21, 8)(3, 17, 20, 10)(4, 6, 7, 5)
R = Permutation(1, 5, 21, 14)(3, 7, 23, 12)(8, 10, 11, 9)
D = Permutation(6, 18, 14, 10)(7, 19, 15, 11)(20, 22, 23, 21)
# These are passed as permutations to PermutationGroup:


G = PermutationGroup(F, R, D)
@test order(G) == 3674160
# 3674160
# The group can be supplied to a Polyhedron in order to track the objects being moved. An example involving the 2x2 Rubik’s cube is given there, but here is a simple demonstration:


a = Permutation(2, 1)
b = Permutation(1, 0)
G = PermutationGroup(a, b)
# P = Polyhedron(list('ABC'), pgroup=G)
# P.corners
# (A, B, C)
# P.rotate(0) # apply permutation 0
# P.corners
# (A, C, B)
# P.reset()
# P.corners
# (A, B, C)
# Or one can make a permutation as a product of selected permutations and apply them to an iterable directly:


P10 = make_perm(G, [0, 1])
@test P10("ABC") == ["C", "A", "B"]
# ['C', 'A', 'B']
# base
# Return a base# from the Schreier-Sims algorithm.

# For a permutation group GG, a base is a sequence of points B=(b1,b2,...,bk)B=(b1,b2,...,bk) such that no element of GG apart# from the identity fixes all the points in BB. The concepts of a base and strong generating set and their applications are discussed in depth in [1], pp. 87-89 and [2], pp. 55-57.

# An alternative way to think of BB is that it gives the indices of the stabilizer cosets that contain more than the identity permutation.

# See also strong_gens, basic_transversals, basic_orbits, basic_stabilizers
# Examples


# from sympy.combinatorics import Permutation, PermutationGroup
G = PermutationGroup([Permutation(0, 1, 3)(2, 4)])
@test    SymPy.base(G) == [0,2]
# [0, 2]


    ## -----

    # baseswap(base, strong_gens, pos, randomized=False, transversals=None, basic_orbits=None, strong_gens_distr=None)[source]
# Swap two consecutive base points in base and strong generating set.

# If a base for a group GG is given by (b1,b2,...,bk)(b1,b2,...,bk), this function returns a base (b1,b2,...,bi+1,bi,...,bk)(b1,b2,...,bi+1,bi,...,bk), where ii is given by pos, and a strong generating set relative to that base. The original base and strong generating set are not modified.

# The randomized version (default) is of Las Vegas type.

# Parameters:	
# base, strong_gens
# The base and strong generating set.
# pos
# The position at which swapping is performed.
# randomized
# A switch between randomized and deterministic version.
# transversals
# The transversals for the basic orbits, if known.
# basic_orbits
# The basic orbits, if known.
# strong_gens_distr
# The strong generators distributed by basic stabilizers, if known.
# Returns:	
# (base, strong_gens)
# base is the new base, and strong_gens is a generating set relative to it.
# See also schreier_sims
# Notes

# The deterministic version of the algorithm is discussed in [1], pp. 102-103; the randomized version is discussed in [1], p.103, and [2], p.98. It is of Las Vegas type. Notice that [1] contains a mistake in the pseudocode and discussion of BASESWAP: on line 3 of the pseudocode, |β⟨T⟩i+1||βi+1⟨T⟩| should be replaced by |β⟨T⟩i||βi⟨T⟩|, and the same for the discussion of the algorithm.

# Examples

# 
# # from sympy.combinatorics.named_groups import SymmetricGroup
# # from sympy.combinatorics.testutil import _verify_bsgs
# # from sympy.combinatorics.perm_groups import PermutationGroup
S = SymmetricGroup(4)
schreier_sims(S)
@test    SymPy.base(S) == [0, 1, 2]
# [0, 1, 2]

_base, gens = baseswap(S, base(S), strong_gens(S), 1, randomized=false)  
@test _base == [0,2,1]
@test gens == [Permutation(0, 1, 2, 3), Permutation(3)(0, 1), Permutation(1, 3, 2),
              Permutation(2, 3), Permutation(1, 3)]
# # ([0, 2, 1],
# # [(0 1 2 3), (3)(0 1), (1 3 2),
# #  (2 3), (1 3)])
# # check that base, gens is a BSGS

# 
# ## XXX--->>>    S1 = PermutationGroup(gens)
# ## XXX--->>>    _verify_bsgs(S1, base, gens)
# True



# basic_orbits
# Return the basic orbits relative to a base and strong generating set.

# If (b1,b2,...,bk)(b1,b2,...,bk) is a base for a group GG, and G(i)=Gb1,b2,...,bi−1G(i)=Gb1,b2,...,bi−1 is the i-th basic stabilizer (so that G(1)=GG(1)=G), the i-th basic orbit relative to this base is the orbit of bibi under G(i)G(i). See [1], pp. 87-89 for more information.

# See also base, strong_gens, basic_transversals, basic_stabilizers
# Examples


# from sympy.combinatorics.named_groups import SymmetricGroup
S = SymmetricGroup(4)
@test    basic_orbits(S) == [[0, 1, 2, 3], [1, 2, 3], [2, 3]]
# [[0, 1, 2, 3], [1, 2, 3], [2, 3]]
# basic_stabilizers
# Return a chain of stabilizers relative to a base and strong generating set.

# The i-th basic stabilizer G(i)G(i) relative to a base (b1,b2,...,bk)(b1,b2,...,bk) is Gb1,b2,...,bi−1Gb1,b2,...,bi−1. For more information, see [1], pp. 87-89.

# See also base, strong_gens, basic_orbits, basic_transversals
# Examples


# from sympy.combinatorics.named_groups import AlternatingGroup
A = AlternatingGroup(4)
schreier_sims(A)
@test   SymPy.base(A) == [0, 1]
# [0, 1]
for g in basic_stabilizers(A)
     g
end
# ...
# PermutationGroup([
#     (3)(0 1 2),
#     (1 2 3)])
# PermutationGroup([
#     (1 2 3)])
# basic_transversals
# Return basic transversals relative to a base and strong generating set.

# The basic transversals are transversals of the basic orbits. They are provided as a list of dictionaries, each dictionary having keys - the elements of one of the basic orbits, and values - the corresponding transversal elements. See [1], pp. 87-89 for more information.

# See also strong_gens, base, basic_orbits, basic_stabilizers
# Examples


# from sympy.combinatorics.named_groups import AlternatingGroup
A = AlternatingGroup(4)
basic_transversals(A)
# [{0: (3), 1: (3)(0 1 2), 2: (3)(0 2 1), 3: (0 3 1)}, {1: (3), 2: (1 2 3), 3: (1 3 2)}]


## -----

# center()[source]
# Return the center of a permutation group.

# The center for a group GG is defined as Z(G)={z∈G|∀g∈G,zg=gz}Z(G)={z∈G|∀g∈G,zg=gz}, the set of elements of GG that commute with all elements of GG. It is equal to the centralizer of GG inside GG, and is naturally a subgroup of GG ([9]).

# See also centralizer
# Notes

# This is a naive implementation that is a straightforward application of .centralizer()

# Examples


# from sympy.combinatorics.perm_groups import PermutationGroup
# from sympy.combinatorics.named_groups import DihedralGroup
D = DihedralGroup(4)
G = center(D)
@test order(G) == 2
# 2


## -----

# centralizer(other)[source]
# Return the centralizer of a group/set/element.

# The centralizer of a set of permutations S inside a group G is the set of elements of G that commute with all elements of S:

# `C_G(S) = \{ g \in G | gs = sg \forall s \in S\}` ([10])
# Usually, S is a subset of G, but if G is a proper subgroup of the full symmetric group, we allow for S to have elements outside G.

# It is naturally a subgroup of G; the centralizer of a permutation group is equal to the centralizer of any set of generators for that group, since any element commuting with the generators commutes with any product of the generators.

# Parameters:	
# other
# a permutation group/list of permutations/single permutation
# See also subgroup_search
# Notes

# The implementation is an application of .subgroup_search() with tests using a specific base for the group G.

# Examples


# from sympy.combinatorics.named_groups import (SymmetricGroup,
# ... CyclicGroup)
S = SymmetricGroup(6)
C = CyclicGroup(6)
H = centralizer(S, C)
@test is_subgroup(H, C)
# True


## -----

# commutator(G, H)[source]
# Return the commutator of two subgroups.

# For a permutation group K and subgroups G, H, the commutator of G and H is defined as the group generated by all the commutators [g,h]=hgh−1g−1[g,h]=hgh−1g−1 for g in G and h in H. It is naturally a subgroup of K ([1], p.27).

# See also derived_subgroup
# Notes

# The commutator of two subgroups H,GH,G is equal to the normal closure of the commutators of all the generators, i.e. hgh−1g−1hgh−1g−1 for hh a generator of HH and gg a generator of GG ([1], p.28)

# Examples


# from sympy.combinatorics.named_groups import (SymmetricGroup,
# ... AlternatingGroup)
S = SymmetricGroup(5)
A = AlternatingGroup(5)
G = commutator(S, S, A)
@test is_subgroup(G, A)
# True


## -----

# contains(g, strict=True)[source]
# Test if permutation g belong to self, G.

# If g is an element of G it can be written as a product of factors drawn# from the cosets of G’s stabilizers. To see if g is one of the actual generators defining the group use G.has(g).

# If strict is not True, g will be resized, if necessary, to match the size of permutations in self.

# See also coset_factor, has, in
# Examples


# from sympy.combinatorics import Permutation
# from sympy.combinatorics.perm_groups import PermutationGroup

a = Permutation(1, 2)
b = Permutation(2, 3, 1)
G = PermutationGroup(a, b, degree=5)
@test contains(G, G[1]) # trivial check
# True
elem = Permutation([[2, 3]], size=5)
@test contains(G, elem)
# True
@test !contains(G, Permutation(4)(0, 1, 2, 3))
# False
# If strict is False, a permutation will be resized, if necessary:


H = PermutationGroup(Permutation(5))
@test !contains(H, Permutation(3))
# False
@test contains(H, Permutation(3), strict=false)
# True
# To test if a given permutation is present in the group:


for elem in generators(G)
    # False
    has(G, elem)
end
# False


## -----

# coset_factor(g, factor_index=False)[source]
# Return G’s (self’s) coset factorization of g

# If g is an element of G then it can be written as the product of permutations drawn# from the Schreier-Sims coset decomposition,

# The permutations returned in f are those for which the product gives g: g = f[n]*...f[1]*f[0] where n = len(B) and B = G.base. f[i] is one of the permutations in self._basic_orbits[i].

# If factor_index==True, returns a tuple [b[0],..,b[n]], where b[i] belongs to self._basic_orbits[i]

# Examples


# from sympy.combinatorics import Permutation, PermutationGroup
 a = Permutation(0, 1, 3, 7, 6, 4)(2, 5)
 b = Permutation(0, 1, 3, 2)(4, 5, 7, 6)
 G = PermutationGroup([a, b])
# Define g:


 g = Permutation(7)(1, 2, 4)(3, 6, 5)
# Confirm that it is an element of G:


 contains(G, g)
# True
# Thus, it can be written as a product of factors (up to 3) drawn# from u. See below that a factor# from u1 and u2 and the Identity permutation have been used:


f = coset_factor(G, g)
@test f[3]*f[2]*f[1] == g
# True
f1 = coset_factor(G, g, true);
f1
# [0, 4, 4]
 tr = basic_transversals(G)
@test f[1] == tr[1][f1[1]]
# True
# If g is not an element of G then [] is returned:


 c = Permutation(5, 6, 7)
@test coset_factor(G, c) == []
# []
# see util._strip



## -----

# coset_rank(g)[source]
# rank using Schreier-Sims representation

# The coset rank of g is the ordering number in which it appears in the lexicographic listing according to the coset decomposition

# The ordering is the same as in G.generate(method=’coset’). If g does not belong to the group it returns None.

# See also coset_factor
# Examples


# from sympy.combinatorics import Permutation
# from sympy.combinatorics.perm_groups import PermutationGroup
 a = Permutation(0, 1, 3, 7, 6, 4)(2, 5)
 b = Permutation(0, 1, 3, 2)(4, 5, 7, 6)
 G = PermutationGroup([a, b])
 c = Permutation(7)(2, 4)(3, 5)
coset_rank(G, c)
# 16
coset_unrank(G,16)
# (7)(2 4)(3 5)


## -----

# coset_table(H)[source]
# Return the standardised (right) coset table of self in H as a list of lists.



## -----

# coset_transversal(H)[source]
# Return a transversal of the right cosets of self by its subgroup H using the second method described in [1], Subsection 4.6.7



## -----

# coset_unrank(rank, af=False)[source]
# unrank using Schreier-Sims representation

# coset_unrank is the inverse operation of coset_rank if 0 <= rank < order; otherwise it returns None.

# degree
# Returns the size of the permutations in the group.

# The number of permutations comprising the group is given by len(group); the number of permutations that can be generated by the group is given by group.order().

# See also order
# Examples


# from sympy.combinatorics import Permutation
# from sympy.combinatorics.perm_groups import PermutationGroup
 a = Permutation([1, 0, 2])
 G = PermutationGroup([a])
degree(G)
# 3
length(G)
# 1
order(G)
# 2
collect(generate(G)) ## list(generate(G)) XXX
# [(2), (2)(0 1)]


## -----

# derived_series()[source]
# Return the derived series for the group.

# The derived series for a group GG is defined as G=G0>G1>G2>…G=G0>G1>G2>… where Gi=[Gi−1,Gi−1]Gi=[Gi−1,Gi−1], i.e. GiGi is the derived subgroup of Gi−1Gi−1, for i∈ℕi∈N. When we have Gk=Gk−1Gk=Gk−1 for some k∈ℕk∈N, the series terminates.

# Returns:	
# A list of permutation groups containing the members of the derived
# series in the order G=G0,G1,G2,…G=G0,G1,G2,….
# See also derived_subgroup
# Examples


# from sympy.combinatorics.named_groups import (SymmetricGroup,
# ... AlternatingGroup, DihedralGroup)
A = AlternatingGroup(5)
@test length(derived_series(A)) == 1
# 1
S = SymmetricGroup(4)
@test length(derived_series(S)) == 4
# 4
@test is_subgroup(derived_series(S)[1+1], AlternatingGroup(4))
# True
@test is_subgroup(derived_series(S)[2+1], DihedralGroup(2))
# True


## -----

# derived_subgroup()[source]
# Compute the derived subgroup.

# The derived subgroup, or commutator subgroup is the subgroup generated by all commutators [g,h]=hgh−1g−1[g,h]=hgh−1g−1 for g,h∈Gg,h∈G ; it is equal to the normal closure of the set of commutators of the generators ([1], p.28, [11]).

# See also derived_series
# Examples


# from sympy.combinatorics import Permutation
# from sympy.combinatorics.perm_groups import PermutationGroup
 a = Permutation([1, 0, 2, 4, 3])
 b = Permutation([0, 1, 3, 2, 4])
 G = PermutationGroup([a, b])
 C = derived_subgroup(G)
collect(generate(C, af=true)) #  list(generate(C, af=true))
# [[0, 1, 2, 3, 4], [0, 1, 3, 4, 2], [0, 1, 4, 2, 3]]
# elements
# Returns all the elements of the permutation group as a set

# Examples


# from sympy.combinatorics import Permutation, PermutationGroup
 p = PermutationGroup(Permutation(1, 3), Permutation(1, 2))
elements(p)
# {(3), (2 3), (3)(1 2), (1 2 3), (1 3 2), (1 3)}


## -----

# generate(method='coset', af=False)[source]
# Return iterator to generate the elements of the group

# Iteration is done with one of these methods:

# method='coset'  using the Schreier-Sims coset representation
# method='dimino' using the Dimino method
# If af = True it yields the array form of the permutations

# Examples


# from sympy.combinatorics import Permutation
# from sympy.combinatorics import PermutationGroup
# from sympy.combinatorics.polyhedron import tetrahedron
# The permutation group given in the tetrahedron object is also true groups:


tet = SymPy.combinatorics[:tetrahedron]
G = PyCall.PyObject(tet)[:pgroup]
#G = tetrahedron.pgroup
@test is_group(G)
# G.is_group
# True
# Also the group generated by the permutations in the tetrahedron pgroup – even the first two – is a proper group:


H = PermutationGroup(G[1], G[2])
J = PermutationGroup(collect(generate(H))); J
# PermutationGroup([
#     (0 1)(2 3),
#     (3),
#     (1 2 3),
#     (1 3 2),
#     (0 3 1),
#     (0 2 3),
#     (0 3)(1 2),
#     (0 1 3),
#     (3)(0 2 1),
#     (0 3 2),
#     (3)(0 1 2),
#     (0 2)(1 3)])
is_group(J)
# True


## -----

# generate_dimino(af=False)[source]
# Yield group elements using Dimino’s algorithm

# If af == True it yields the array form of the permutations

# References

# [1] The Implementation of Various Algorithms for Permutation Groups in the Computer Algebra System: AXIOM, N.J. Doye, M.Sc. Thesis

# Examples


# from sympy.combinatorics import Permutation
# from sympy.combinatorics.perm_groups import PermutationGroup
a = Permutation([0, 2, 1, 3])
b = Permutation([0, 2, 3, 1])
g = PermutationGroup([a, b])
@test collect(generate_dimino(g, af=true)) ==  [[0, 1, 2, 3], [0, 2, 1, 3], [0, 2, 3, 1],
                                          [0, 1, 3, 2], [0, 3, 2, 1], [0, 3, 1, 2]]

# [[0, 1, 2, 3], [0, 2, 1, 3], [0, 2, 3, 1],
#  [0, 1, 3, 2], [0, 3, 2, 1], [0, 3, 1, 2]]


## -----

# generate_schreier_sims(af=False)[source]
# Yield group elements using the Schreier-Sims representation in coset_rank order

# If af = True it yields the array form of the permutations

# Examples


# from sympy.combinatorics import Permutation
# from sympy.combinatorics.perm_groups import PermutationGroup
a = Permutation([0, 2, 1, 3])
b = Permutation([0, 2, 3, 1])
g = PermutationGroup([a, b])
collect(generate_schreier_sims(g, af=true))
# [[0, 1, 2, 3], [0, 2, 1, 3], [0, 3, 2, 1],
#  [0, 1, 3, 2], [0, 2, 3, 1], [0, 3, 1, 2]]
# generators
# Returns the generators of the group.

# Examples


# from sympy.combinatorics import Permutation
# from sympy.combinatorics.perm_groups import PermutationGroup
a = Permutation([0, 2, 1])
b = Permutation([1, 0, 2])
G = PermutationGroup([a, b])
generators(G)
# [(1 2), (2)(0 1)]
# is_abelian
# Test if the group is Abelian.

# Examples


# from sympy.combinatorics import Permutation
# from sympy.combinatorics.perm_groups import PermutationGroup
a = Permutation([0, 2, 1])
b = Permutation([1, 0, 2])
G = PermutationGroup([a, b])
@test !is_abelian(G)
# False
a = Permutation([0, 2, 1])
G = PermutationGroup([a])
@test is_abelian(G)
# True


## -----

# is_alt_sym(eps=0.05, _random_prec=None)[source]
# Monte Carlo test for the symmetric/alternating group for degrees >= 8.

# More specifically, it is one-sided Monte Carlo with the answer True (i.e., G is symmetric/alternating) guaranteed to be correct, and the answer False being incorrect with probability eps.

# See also _check_cycles_alt_sym
# Notes

# The algorithm itself uses some nontrivial results# from group theory and number theory: 1) If a transitive group G of degree n contains an element with a cycle of length n/2 < p < n-2 for p a prime, G is the symmetric or alternating group ([1], pp. 81-82) 2) The proportion of elements in the symmetric/alternating group having the property described in 1) is approximately log(2)/log(n)log⁡(2)/log⁡(n) ([1], p.82; [2], pp. 226-227). The helper function _check_cycles_alt_sym is used to go over the cycles in a permutation and look for ones satisfying 1).

# Examples


# from sympy.combinatorics.perm_groups import PermutationGroup
# from sympy.combinatorics.named_groups import DihedralGroup
D = DihedralGroup(10)
@test !is_alt_sym(D)
# False
# is_nilpotent
# Test if the group is nilpotent.

# A group GG is nilpotent if it has a central series of finite length. Alternatively, GG is nilpotent if its lower central series terminates with the trivial group. Every nilpotent group is also solvable ([1], p.29, [12]).

# See also lower_central_series, is_solvable
# Examples


# from sympy.combinatorics.named_groups import (SymmetricGroup,
# ... CyclicGroup)
C = CyclicGroup(6)
@test is_nilpotent(C)
# True
S = SymmetricGroup(5)
@test !is_nilpotent(S)
# False


## -----

# is_normal(gr, strict=True)[source]
# Test if G=self is a normal subgroup of gr.

# G is normal in gr if for each g2 in G, g1 in gr, g = g1*g2*g1**-1 belongs to G It is sufficient to check this for each g1 in gr.generators and g2 in G.generators.

# Examples


# from sympy.combinatorics import Permutation
# from sympy.combinatorics.perm_groups import PermutationGroup
a = Permutation([1, 2, 0])
b = Permutation([1, 0, 2])
G = PermutationGroup([a, b])
G1 = PermutationGroup([a, Permutation([2, 0, 1])])
@test is_normal(G1, G)
# True



## -----

# is_primitive(randomized=True)[source]
# Test if a group is primitive.

# A permutation group G acting on a set S is called primitive if S contains no nontrivial block under the action of G (a block is nontrivial if its cardinality is more than 1).

# See also minimal_block, random_stab
# Notes

# The algorithm is described in [1], p.83, and uses the function minimal_block to search for blocks of the form {0,k}{0,k} for k ranging over representatives for the orbits of G0G0, the stabilizer of 0. This algorithm has complexity O(n2)O(n2) where n is the degree of the group, and will perform badly if G0G0 is small.

# There are two implementations offered: one finds G0G0 deterministically using the function stabilizer, and the other (default) produces random elements of G0G0 using random_stab, hoping that they generate a subgroup of G0G0 with not too many more orbits than G0G0 (this is suggested in [1], p.83). Behavior is changed by the randomized flag.

# Examples


# from sympy.combinatorics.perm_groups import PermutationGroup
# from sympy.combinatorics.named_groups import DihedralGroup
D = DihedralGroup(10)
@test !is_primitive(D)
# False
# is_solvable
# Test if the group is solvable.

# G is solvable if its derived series terminates with the trivial group ([1], p.29).

# See also is_nilpotent, derived_series
# Examples


# from sympy.combinatorics.named_groups import SymmetricGroup
S = SymmetricGroup(3)
@test is_solvable(S)
# True



## -----

# is_subgroup(G, strict=True)[source]
# Return True if all elements of self belong to G.

# If strict is False then if self’s degree is smaller than G’s, the elements will be resized to have the same degree.

# Examples


# from sympy.combinatorics import Permutation, PermutationGroup
# from sympy.combinatorics.named_groups import (SymmetricGroup,
# ...    CyclicGroup)
# Testing is strict by default: the degree of each group must be the same:


p = Permutation(0, 1, 2, 3, 4, 5)
G1 = PermutationGroup([Permutation(0, 1, 2), Permutation(0, 1)])
G2 = PermutationGroup([Permutation(0, 2), Permutation(0, 1, 2)])
G3 = PermutationGroup([p, p^2])
@test order(G1) == order(G2) == order(G3) == 6
@test is_subgroup(G1, G2)
# True
@test !is_subgroup(G1, G3)
# False
@test !is_subgroup(G1, PermutationGroup(G3[2]))
# False
@test is_subgroup(G3, PermutationGroup(G3[1]))
# True
# To ignore the size, set strict to False:


S3 = SymmetricGroup(3)
S5 = SymmetricGroup(5)
@test is_subgroup(S3, S5, strict=false)
# True
C7 = CyclicGroup(7)
G = S5*C7
@test is_subgroup(S5, G, false)
# True
@test !is_subgroup(C7, G, 0)
# False




## -----

# is_transitive(strict=True)[source]
# Test if the group is transitive.

# A group is transitive if it has a single orbit.

# If strict is False the group is transitive if it has a single orbit of length different# from 1.

# Examples


# from sympy.combinatorics.permutations import Permutation
# from sympy.combinatorics.perm_groups import PermutationGroup
a = Permutation([0, 2, 1, 3])
b = Permutation([2, 0, 1, 3])
G1 = PermutationGroup([a, b])
@test !is_transitive(G1)
# False
@test is_transitive(G1, strict=false)
# True
c = Permutation([2, 3, 0, 1])
G2 = PermutationGroup([a, c])
@test is_transitive(G2)
# True
d = Permutation([1, 0, 2, 3])
e = Permutation([0, 1, 3, 2])
G3 = PermutationGroup([d, e])
@test !(is_transitive(G3) || is_transitive(G3, strict=false))
# False
# is_trivial
# Test if the group is the trivial group.

# This is true if the group contains only the identity permutation.

# Examples


# from sympy.combinatorics import Permutation
# from sympy.combinatorics.perm_groups import PermutationGroup
G = PermutationGroup([Permutation([0, 1, 2])])
@test is_trivial(G)
# True


## -----

# lower_central_series()[source]
# Return the lower central series for the group.

# The lower central series for a group GG is the series G=G0>G1>G2>…G=G0>G1>G2>… where Gk=[G,Gk−1]Gk=[G,Gk−1], i.e. every term after the first is equal to the commutator of GG and the previous term in G1G1 ([1], p.29).

# Returns:	A list of permutation groups in the order G=G0,G1,G2,…G=G0,G1,G2,…
# See also commutator, derived_series
# Examples


# from sympy.combinatorics.named_groups import (AlternatingGroup,
# ... DihedralGroup)
A = AlternatingGroup(4)
@test length(lower_central_series(A))== 2
# 2
@test is_subgroup(lower_central_series(A)[2], DihedralGroup(2))
# True




## -----

# make_perm(n, seed=None)[source]
# Multiply n randomly selected permutations# from pgroup together, starting with the identity permutation. If n is a list of integers, those integers will be used to select the permutations and they will be applied in L to R order: make_perm((A, B, C)) will give CBA(I) where I is the identity permutation.

# seed is used to set the seed for the random selection of permutations# from pgroup. If this is a list of integers, the corresponding permutations# from pgroup will be selected in the order give. This is mainly used for testing purposes.

# See also random
# Examples


# from sympy.combinatorics import Permutation
# from sympy.combinatorics.perm_groups import PermutationGroup
a, b = [Permutation([1, 0, 3, 2]), Permutation([1, 3, 0, 2])]
G = PermutationGroup([a, b])
@test make_perm(G, 1, [0]) ==  Permutation(0,1)(2,3)
# (0 1)(2 3)
@test make_perm(G, 3, [0, 1, 0]) == Permutation(0,2,3,1)
# (0 2 3 1)
@test make_perm(G, [0, 1, 0]) == Permutation(0,2,3,1)
# (0 2 3 1)

# max_div
# Maximum proper divisor of the degree of a permutation group.

# See also minimal_block, _union_find_merge
# Notes

# Obviously, this is the degree divided by its minimal proper divisor (larger than 1, if one exists). As it is guaranteed to be prime, the sieve# from sympy.ntheory is used. This function is also used as an optimization tool for the functions minimal_block and _union_find_merge.

# Examples


# from sympy.combinatorics import Permutation
# from sympy.combinatorics.perm_groups import PermutationGroup
G = PermutationGroup([Permutation([0, 2, 1, 3])])
@test max_div(G) == 2
# 2


## -----

# minimal_block(points)[source]
# For a transitive group, finds the block system generated by points.

# If a group G acts on a set S, a nonempty subset B of S is called a block under the action of G if for all g in G we have gB = B (g fixes B) or gB and B have no common points (g moves B entirely). ([1], p.23; [6]).

# The distinct translates gB of a block B for g in G partition the set S and this set of translates is known as a block system. Moreover, we obviously have that all blocks in the partition have the same size, hence the block size divides |S| ([1], p.23). A G-congruence is an equivalence relation ~ on the set S such that a ~ b implies g(a) ~ g(b) for all g in G. For a transitive group, the equivalence classes of a G-congruence and the blocks of a block system are the same thing ([1], p.23).

# The algorithm below checks the group for transitivity, and then finds the G-congruence generated by the pairs (p_0, p_1), (p_0, p_2), ..., (p_0,p_{k-1}) which is the same as finding the maximal block system (i.e., the one with minimum block size) such that p_0, ..., p_{k-1} are in the same block ([1], p.83).

# It is an implementation of Atkinson’s algorithm, as suggested in [1], and manipulates an equivalence relation on the set S using a union-find data structure. The running time is just above O(|points||S|)O(|points||S|). ([1], pp. 83-87; [7]).

# See also _union_find_rep, _union_find_merge, is_transitive, is_primitive
# Examples


# from sympy.combinatorics.perm_groups import PermutationGroup
# from sympy.combinatorics.named_groups import DihedralGroup
D = DihedralGroup(10)
@test minimal_block(D, [0, 5]) == [0, 6, 2, 8, 4, 0, 6, 2, 8, 4]
# [0, 6, 2, 8, 4, 0, 6, 2, 8, 4]
@test minimal_block(D, [0, 1]) == [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
# [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]




## -----

# normal_closure(other, k=10)[source]
# Return the normal closure of a subgroup/set of permutations.

# If S is a subset of a group G, the normal closure of A in G is defined as the intersection of all normal subgroups of G that contain A ([1], p.14). Alternatively, it is the group generated by the conjugates x^{-1}yx for x a generator of G and y a generator of the subgroup \left\langle S\right\rangle generated by S (for some chosen generating set for \left\langle S\right\rangle) ([1], p.73).

# Parameters:	
# other
# a subgroup/list of permutations/single permutation
# k
# an implementation-specific parameter that determines the number of conjugates that are adjoined to other at once
# See also commutator, derived_subgroup, random_pr
# Notes

# The algorithm is described in [1], pp. 73-74; it makes use of the generation of random elements for permutation groups by the product replacement algorithm.

# Examples


# from sympy.combinatorics.named_groups import (SymmetricGroup,
# ... CyclicGroup, AlternatingGroup)
S = SymmetricGroup(5)
C = CyclicGroup(5)
G = normal_closure(S, C)
@test order(G) == 60
# 60
@test is_subgroup(G, AlternatingGroup(5))
# True




## -----

# orbit(alpha, action='tuples')[source]
# Compute the orbit of alpha {g(α)|g∈G}{g(α)|g∈G} as a set.

# The time complexity of the algorithm used here is O(|Orb|∗r)O(|Orb|∗r) where |Orb||Orb| is the size of the orbit and r is the number of generators of the group. For a more detailed analysis, see [1], p.78, [2], pp. 19-21. Here alpha can be a single point, or a list of points.

# If alpha is a single point, the ordinary orbit is computed. if alpha is a list of points, there are three available options:

# ‘union’ - computes the union of the orbits of the points in the list ‘tuples’ - computes the orbit of the list interpreted as an ordered tuple under the group action ( i.e., g((1,2,3)) = (g(1), g(2), g(3)) ) ‘sets’ - computes the orbit of the list interpreted as a sets

# See also orbit_transversal
# Examples


# from sympy.combinatorics import Permutation
# from sympy.combinatorics.perm_groups import PermutationGroup
a = Permutation([1, 2, 0, 4, 5, 6, 3])
G = PermutationGroup([a])
@test Set(orbit(G, 0)) == Set([0,1,2])
# {0, 1, 2}

@test Set(orbit(G, [0, 4], "union")) == Set([0, 1, 2, 3, 4, 5, 6])  
# {0, 1, 2, 3, 4, 5, 6}




## -----

# orbit_rep(alpha, beta, schreier_vector=None)[source]
# Return a group element which sends alpha to beta.

# If beta is not in the orbit of alpha, the function returns False. This implementation makes use of the schreier vector. For a proof of correctness, see [1], p.80

# See also schreier_vector
# Examples


# from sympy.combinatorics import Permutation
# from sympy.combinatorics.perm_groups import PermutationGroup
# from sympy.combinatorics.named_groups import AlternatingGroup
G = AlternatingGroup(5)
@test orbit_rep(G, 0, 4) == Permutation(0,4,1,2,3)
# (0 4 1 2 3)


## -----

# orbit_transversal(alpha, pairs=False)[source]
# Computes a transversal for the orbit of alpha as a set.

# For a permutation group GG, a transversal for the orbit Orb={g(α)|g∈G}Orb={g(α)|g∈G} is a set {gβ|gβ(α)=β}{gβ|gβ(α)=β} for β∈Orbβ∈Orb. Note that there may be more than one possible transversal. If pairs is set to True, it returns the list of pairs (β,gβ)(β,gβ). For a proof of correctness, see [1], p.79

# See also orbit
# Examples


# from sympy.combinatorics import Permutation
# from sympy.combinatorics.perm_groups import PermutationGroup
# from sympy.combinatorics.named_groups import DihedralGroup
G = DihedralGroup(6)
@test orbit_transversal(G, 0) == [Permutation(5), Permutation(0, 1, 2, 3, 4, 5), Permutation(0, 5)(1, 4)(2, 3), Permutation(0, 2, 4)(1, 3, 5), Permutation(5)(0, 4)(1, 3), Permutation(0, 3)(1, 4)(2, 5)]
# [(5), (0 1 2 3 4 5), (0 5)(1 4)(2 3), (0 2 4)(1 3 5), (5)(0 4)(1 3), (0 3)(1 4)(2 5)]




## -----

# orbits(rep=False)[source]
# Return the orbits of self, ordered according to lowest element in each orbit.

# Examples


# from sympy.combinatorics.permutations import Permutation
# from sympy.combinatorics.perm_groups import PermutationGroup
a = Permutation(1, 5)(2, 3)(4, 0, 6)
b = Permutation(1, 5)(3, 4)(2, 6, 0)
G = PermutationGroup([a, b])
@test collect.(orbits(G)) == [[0,2,3,4,6], [1,5]] ## XXX Collect first
# [{0, 2, 3, 4, 6}, {1, 5}]




## -----

# order()[source]
# Return the order of the group: the number of permutations that can be generated# from elements of the group.

# The number of permutations comprising the group is given by len(group); the length of each permutation in the group is given by group.size.

# See also degree
# Examples


# from sympy.combinatorics.permutations import Permutation
# from sympy.combinatorics.perm_groups import PermutationGroup

a = Permutation([1, 0, 2])
G = PermutationGroup([a])
@test degree(G) == 3
# 3
@test len(G) == 1
# 1
@test order(G) == 2
# 2
# list(G.generate())
@test collect(generate(G)) == [Permutation(2), Permutation(2)(0,1)]
# [(2), (2)(0 1)]

a = Permutation([0, 2, 1])
b = Permutation([1, 0, 2])
G = PermutationGroup([a, b])
@test order(G) == 6
# 6


## -----

# pointwise_stabilizer(points, incremental=True)[source]
# Return the pointwise stabilizer for a set of points.

# For a permutation group GG and a set of points {p1,p2,…,pk}{p1,p2,…,pk}, the pointwise stabilizer of p1,p2,…,pkp1,p2,…,pk is defined as Gp1,…,pk={g∈G|g(pi)=pi∀i∈{1,2,…,k}}Gp1,…,pk={g∈G|g(pi)=pi∀i∈{1,2,…,k}} ([1],p20). It is a subgroup of GG.

# See also stabilizer, schreier_sims_incremental
# Notes

# When incremental == True, rather than the obvious implementation using successive calls to .stabilizer(), this uses the incremental Schreier-Sims algorithm to obtain a base with starting segment - the given points.

# Examples


# from sympy.combinatorics.named_groups import SymmetricGroup
S = SymmetricGroup(7)
Stab = pointwise_stabilizer(S, [2, 3, 5]) ## note PyVector usage
#Stab.is_subgroup(S.stabilizer(2).stabilizer(3).stabilizer(5))
@test is_subgroup(Stab, stabilizer(S, 2) |> S -> stabilizer(S, 3) |> S -> stabilizer(S, 5))
# True




## -----

# random(af=False)[source]
# Return a random group element



## -----

# random_pr(gen_count=11, iterations=50, _random_prec=None)[source]
# Return a random group element using product replacement.

# For the details of the product replacement algorithm, see _random_pr_init In random_pr the actual ‘product replacement’ is performed. Notice that if the attribute _random_gens is empty, it needs to be initialized by _random_pr_init.

# See also _random_pr_init


## -----

# random_stab(alpha, schreier_vector=None, _random_prec=None)[source]
# Random element# from the stabilizer of alpha.

# The schreier vector for alpha is an optional argument used for speeding up repeated calls. The algorithm is described in [1], p.81

# See also random_pr, orbit_rep


## -----

# schreier_sims()[source]
# Schreier-Sims algorithm.

# It computes the generators of the chain of stabilizers G>Gb1>..>Gb1,..,br>1G>Gb1>..>Gb1,..,br>1 in which Gb1,..,biGb1,..,bi stabilizes b1,..,bib1,..,bi, and the corresponding s cosets. An element of the group can be written as the product h1∗..∗hsh1∗..∗hs.

# We use the incremental Schreier-Sims algorithm.

# Examples


# from sympy.combinatorics.permutations import Permutation
# from sympy.combinatorics.perm_groups import PermutationGroup
a = Permutation([0, 2, 1])
b = Permutation([1, 0, 2])
G = PermutationGroup([a, b])
schreier_sims(G)
basic_transversals(G)
# [{0: (2)(0 1), 1: (2), 2: (1 2)},
#  {0: (2), 2: (0 2)}]


## -----

# schreier_sims_incremental(base=None, gens=None)[source]
# Extend a sequence of points and generating set to a base and strong generating set.

# Parameters:	
# base
# The sequence of points to be extended to a base. Optional parameter with default value [].
# gens
# The generating set to be extended to a strong generating set relative to the base obtained. Optional parameter with default value self.generators.
# Returns:	
# (base, strong_gens)
# base is the base obtained, and strong_gens is the strong generating set relative to it. The original parameters base, gens remain unchanged.
# See also schreier_sims, schreier_sims_random
# Notes

# This version of the Schreier-Sims algorithm runs in polynomial time. There are certain assumptions in the implementation - if the trivial group is provided, base and gens are returned immediately, as any sequence of points is a base for the trivial group. If the identity is present in the generators gens, it is removed as it is a redundant generator. The implementation is described in [1], pp. 90-93.

# Examples


# from sympy.combinatorics.named_groups import AlternatingGroup
# from sympy.combinatorics.perm_groups import PermutationGroup
# from sympy.combinatorics.testutil import _verify_bsgs
A = AlternatingGroup(7)
_bas3 = [2, 3]
#seq = PyCall.PyVector([2, 3])
seq = [2,3]
_base, _strong_gens = schreier_sims_incremental(A, base=seq)
# _verify_bsgs(A, base, _strong_gens)
# True
_base[2]
# [2, 3]





## -----

# schreier_sims_random(base=None, gens=None, consec_succ=10, _random_prec=None)[source]
# Randomized Schreier-Sims algorithm.

# The randomized Schreier-Sims algorithm takes the sequence base and the generating set gens, and extends base to a base, and gens to a strong generating set relative to that base with probability of a wrong answer at most 2−consec_succ2−consec_succ, provided the random generators are sufficiently random.

# Parameters:	
# base
# The sequence to be extended to a base.
# gens
# The generating set to be extended to a strong generating set.
# consec_succ
# The parameter defining the probability of a wrong answer.
# _random_prec
# An internal parameter used for testing purposes.
# Returns:	
# (base, strong_gens)
# base is the base and strong_gens is the strong generating set relative to it.
# See also schreier_sims
# Notes

# The algorithm is described in detail in [1], pp. 97-98. It extends the orbits orbs and the permutation groups stabs to basic orbits and basic stabilizers for the base and strong generating set produced in the end. The idea of the extension process is to “sift” random group elements through the stabilizer chain and amend the stabilizers/orbits along the way when a sift is not successful. The helper function _strip is used to attempt to decompose a random group element according to the current state of the stabilizer chain and report whether the element was fully decomposed (successful sift) or not (unsuccessful sift). In the latter case, the level at which the sift failed is reported and used to amend stabs, base, gens and orbs accordingly. The halting condition is for consec_succ consecutive successful sifts to pass. This makes sure that the current base and gens form a BSGS with probability at least 1−1/consec\_succ1−1/consec\_succ.

# Examples


# from sympy.combinatorics.perm_groups import PermutationGroup
# from sympy.combinatorics.testutil import _verify_bsgs
# from sympy.combinatorics.named_groups import SymmetricGroup
S = SymmetricGroup(5)
_base, _strong_gens = schreier_sims_random(S, consec_succ=5)
## _verify_bsgs(S, _base, _strong_gens) 
# True




## -----

# schreier_vector(alpha)[source]
# Computes the schreier vector for alpha.

# The Schreier vector efficiently stores information about the orbit of alpha. It can later be used to quickly obtain elements of the group that send alpha to a particular element in the orbit. Notice that the Schreier vector depends on the order in which the group generators are listed. For a definition, see [3]. Since list indices start# from zero, we adopt the convention to use “None” instead of 0 to signify that an element doesn’t belong to the orbit. For the algorithm and its correctness, see [2], pp.78-80.

# See also orbit
# Examples


# from sympy.combinatorics.perm_groups import PermutationGroup
# from sympy.combinatorics.permutations import Permutation
a = Permutation([2, 4, 6, 3, 1, 5, 0])
b = Permutation([0, 1, 3, 5, 4, 6, 2])
G = PermutationGroup([a, b])
@test schreier_vector(G, 0) == [-1, nothing, 0, 1, nothing, 1, 0]
# [-1, None, 0, 1, None, 1, 0]




## -----

# stabilizer(alpha)[source]
# Return the stabilizer subgroup of alpha.

# The stabilizer of αα is the group Gα={g∈G|g(α)=α}Gα={g∈G|g(α)=α}. For a proof of correctness, see [1], p.79.

# See also orbit
# Examples


# from sympy.combinatorics import Permutation
# from sympy.combinatorics.perm_groups import PermutationGroup
# from sympy.combinatorics.named_groups import DihedralGroup
G = DihedralGroup(6)
@test elements(stabilizer(G, 5)) == elements(PermutationGroup(Permutation(5)(0,4)(1,3), Permutation(5)))
# PermutationGroup([
#     (5)(0 4)(1 3),
#     (5)])

# strong_gens
# Return a strong generating set# from the Schreier-Sims algorithm.

# A generating set S={g1,g2,...,gt}S={g1,g2,...,gt} for a permutation group GG is a strong generating set relative to the sequence of points (referred to as a “base”) (b1,b2,...,bk)(b1,b2,...,bk) if, for 1≤i≤k1≤i≤k we have that the intersection of the pointwise stabilizer G(i+1):=Gb1,b2,...,biG(i+1):=Gb1,b2,...,bi with SS generates the pointwise stabilizer G(i+1)G(i+1). The concepts of a base and strong generating set and their applications are discussed in depth in [1], pp. 87-89 and [2], pp. 55-57.

# See also base, basic_transversals, basic_orbits, basic_stabilizers
# Examples


# from sympy.combinatorics.named_groups import DihedralGroup
D = DihedralGroup(4)
@test strong_gens(D)  == [Permutation(0, 1, 2, 3), Permutation(0, 3)(1, 2), Permutation(1, 3)]
# [(0 1 2 3), (0 3)(1 2), (1 3)]
@test SymPy.base(D) == [0,1]
# [0, 1]


## -----

# subgroup(gens)[source]
# Return the subgroup generated by gensgens which is a list of elements of the group



## -----

# subgroup_search(prop, base=None, strong_gens=None, tests=None, init_subgroup=None)[source]
# Find the subgroup of all elements satisfying the property prop.

# This is done by a depth-first search with respect to base images that uses several tests to prune the search tree.

# Parameters:	
# prop
# The property to be used. Has to be callable on group elements and always return True or False. It is assumed that all group elements satisfying prop indeed form a subgroup.
# base
# A base for the supergroup.
# strong_gens
# A strong generating set for the supergroup.
# tests
# A list of callables of length equal to the length of base. These are used to rule out group elements by partial base images, so that tests[l](g) returns False if the element g is known not to satisfy prop base on where g sends the first l + 1 base points.
# init_subgroup
# if a subgroup of the sought group is known in advance, it can be passed to the function as this parameter.
# Returns:	
# res
# The subgroup of all elements satisfying prop. The generating set for this group is guaranteed to be a strong generating set relative to the base base.
# Notes

# This function is extremely lenghty and complicated and will require some careful attention. The implementation is described in [1], pp. 114-117, and the comments for the code here follow the lines of the pseudocode in the book for clarity.

# The complexity is exponential in general, since the search process by itself visits all members of the supergroup. However, there are a lot of tests which are used to prune the search tree, and users can define their own tests via the tests parameter, so in practice, and for some computations, it’s not terrible.

# A crucial part in the procedure is the frequent base change performed (this is line 11 in the pseudocode) in order to obtain a new basic stabilizer. The book mentiones that this can be done by using .baseswap(...), however the current imlementation uses a more straightforward way to find the next basic stabilizer - calling the function .stabilizer(...) on the previous basic stabilizer.

# Examples


# from sympy.combinatorics.named_groups import (SymmetricGroup,
# ... AlternatingGroup)
# from sympy.combinatorics.perm_groups import PermutationGroup
# from sympy.combinatorics.testutil import _verify_bsgs
S = SymmetricGroup(7)

## Need to pass a callback! <<<---XXX

# prop_even = lambda x: x.is_even
# _base, _strong_gens = schreier_sims_incremental(S)
#G = subgroup_search(S, prop_even, base=_base, strong_gens=_strong_gens)
# is_subgroup(G, AlternatingGroup(7))
# True
# _verify_bsgs(G, base, generators(G))
# True
# transitivity_degree
# Compute the degree of transitivity of the group.

# A permutation group GG acting on Ω={0,1,...,n−1}Ω={0,1,...,n−1} is k-fold transitive, if, for any k points (a1,a2,...,ak)∈Ω(a1,a2,...,ak)∈Ω and any k points (b1,b2,...,bk)∈Ω(b1,b2,...,bk)∈Ω there exists g∈Gg∈G such that g(a1)=b1,g(a2)=b2,...,g(ak)=bkg(a1)=b1,g(a2)=b2,...,g(ak)=bk The degree of transitivity of GG is the maximum k such that GG is k-fold transitive. ([8])

# See also is_transitive, orbit
# Examples

# from sympy.combinatorics.perm_groups import PermutationGroup
# from sympy.combinatorics.permutations import Permutation
a = Permutation([1, 2, 0])
b = Permutation([1, 0, 2])
G = PermutationGroup([a, b])
@test transitivity_degree(G) == 3
# 3    


end
