# Matrices

[From](https://docs.sympy.org/latest/tutorial/matrices.html)

```python
    >>> from sympy import *
    >>> init_printing(use_unicode=True)
```

```@setup matrices
using SymPy
sympy.init_printing(use_unicode=True)
```

##### In `Julia`:

* In `SymPy`, matrices can be store using `Julia`'s *generic*
  `Matrix{T}` type where `T <: Sym` *or* using SymPy's matrix type,
  wrapped in a `SymMatrix` type by `SymPy`. This tutorial shows
  how to use the underlying `SymMatrix` values. To construct a matrix
  of symbolic values is identical to construction a matrix of numeric
  values within `Julia`, and will be illustrated at the end.


* methods for `SymMatrix` objects use the dot call syntax. As a
  convenience, this will also work for `Array{Sym}` objects. The
  returned value may be a `SymMatrix`, not an `Array{Sym}`.

* The matrix constructor in SymPy using a vector of row vectors does *not* work in `SymPy`, as of newer versions (it does not work with version 1.5.1 of sympy and PyCall). This style is used in this document. The user of `SymPy` can eaesily avoid this specification, using Julia's matrix construction techniques.


```jldoctest matrices
julia> using SymPy

julia> using LinearAlgebra

```

----

To make a matrix in SymPy, use the `Matrix` object.  A matrix is constructed
by providing a list of row vectors that make up the matrix.  For example,
to construct the matrix

$$~
   \left[\begin{array}{cc}1 & -1\\3 & 4\\0 & 2\end{array}\right]
~$$

use

```python
    >>> Matrix([[1, -1], [3, 4], [0, 2]])
    ⎡1  -1⎤
    ⎢     ⎥
    ⎢3  4 ⎥
    ⎢     ⎥
    ⎣0  2 ⎦
```

##### In `Julia`:

* In `Julia`, the `Matrix` constructor is *not* exported, so must be qualified. Here we *avoid* the vector of row vectors above:

```jldoctest matrices
julia> sympy.Matrix([1 -1; 3 4; 0 2])
3×2 Array{Sym,2}:
 1  -1
 3   4
 0   2

```

*However*, through the magic of `PyCall`, such matrices are converted into `Julia` matrices, of type `Array{Sym}`, so the familiar matrix operations for `Julia` users are available.


In fact, the above could be done in the more `Julia`n manner through

```jldoctest matrices
julia> Sym[1 -1; 3 4; 0 2]
3×2 Array{Sym,2}:
 1  -1
 3   4
 0   2

```

using an annotation to ensure the type. Alternatively, through promotion, just a single symbolic object will result in the same:

```jldoctest matrices
julia> [Sym(1) -1; 3 4; 0 2]
3×2 Array{Sym,2}:
 1  -1
 3   4
 0   2

```

----

To make it easy to make column vectors, a list of elements is considered to be
a column vector.

```python
    >>> Matrix([1, 2, 3])
    ⎡1⎤
    ⎢ ⎥
    ⎢2⎥
    ⎢ ⎥
    ⎣3⎦
```

##### In `Julia`:

For ths use, `sympy.Matrix` does work, but again its usage is discouraged:

```jldoctest matrices
julia> sympy.Matrix([1, 2, 3])
3×1 Array{Sym,2}:
 1
 2
 3

```

* Again, this is converted into a `Vector{Sym}` object or entered directly:

```jldoctest matrices
julia> Sym[1,2,3]
3-element Array{Sym,1}:
 1
 2
 3

```

!!!  note "Avoid `sympy.Matrix`"
     As shown, it is better to  avoid  the   `sympy.Matrix` constructor when possible, and when not, only pass in a  symbolic  array created through `Julia`'s array semantics.


----

Matrices are manipulated just like any other object in SymPy or Python.


```python
    >>> M = Matrix([[1, 2, 3], [3, 2, 1]])
    >>> N = Matrix([0, 1, 1])
    >>> M*N
    ⎡5⎤
    ⎢ ⎥
    ⎣3⎦
```

##### In `Julia`:

* In `Julia`, matrices are just matrices, and inherit all of the operations defined on them:

```jldoctest matrices
julia> M = Sym[1 2 3; 3 2 1]
2×3 Array{Sym,2}:
 1  2  3
 3  2  1

julia> N = Sym[0, 1, 1]
3-element Array{Sym,1}:
 0
 1
 1

julia> M*N
2-element Array{Sym,1}:
 5
 3

```

----

One important thing to note about SymPy matrices is that, unlike every other
object in SymPy, they are mutable.  This means that they can be modified in
place, as we will see below.  The downside to this is that `Matrix` cannot
be used in places that require immutability, such as inside other SymPy
expressions or as keys to dictionaries.  If you need an immutable version of
`Matrix`, use `ImmutableMatrix`.

##### In `Julia`:

A distinction is made between `ImmutableMatrix` and a mutable one. Mutable ones are mapped to `Julia` arrays, immutable ones are left as a symbolic object of type `SymMatrix`. The usual infix mathematical operations (but not dot broadcasting), 0-based indexing, and dot call syntax for methods maay  be used with these objects.



## Basic Operations


### Shape


Here are some basic operations on `Matrix`.  To get the shape of a matrix
use `shape`

```python
    >>> M = Matrix([[1, 2, 3], [-2, 0, 4]])
    >>> M
    ⎡1   2  3⎤
    ⎢        ⎥
    ⎣-2  0  4⎦
    >>> M.shape
    (2, 3)
```

##### In `Julia`:

```jldoctest matrices
julia> M = Sym[1 2 3; -2 0 4]
2×3 Array{Sym,2}:
  1  2  3
 -2  0  4

julia> M.shape
(2, 3)

```

Or, the `Julia`n counterpart:

```jldoctest matrices
julia> size(M)
(2, 3)

```

----

### Accessing Rows and Columns


To get an individual row or column of a matrix, use `row` or `col`.  For
example, `M.row(0)` will get the first row. `M.col(-1)` will get the last
column.

```python
    >>> M.row(0)
    [1  2  3]
    >>> M.col(-1)
    ⎡3⎤
    ⎢ ⎥
    ⎣4⎦
```

##### In `Julia`:

* these 0-based operations are supported:

```jldoctest matrices
julia> M.row(0)
1×3 Array{Sym,2}:
 1  2  3

julia> M.col(-1)
2×1 Array{Sym,2}:
 3
 4

```

The more familiar counterparts would be:

```jldoctest matrices
julia> M[1,:], M[:, end]
(Sym[1, 2, 3], Sym[3, 4])

```


----

### Deleting and Inserting Rows and Columns


To delete a row or column, use `row_del` or `col_del`.  These operations
will modify the Matrix **in place**.

```python
    >>> M.col_del(0)
    >>> M
    ⎡2  3⎤
    ⎢    ⎥
    ⎣0  4⎦
    >>> M.row_del(1)
    >>> M
    [2  3]
```

##### In `Julia`:

These methods do **not** work on `Array{Sym}` objects, use `Julia's` indexing notation to remove a row or column.

However, these methods **do** work on the `ImmutableMatrix` class:

```jldoctest matrices
julia> M = sympy.ImmutableMatrix([1 2 3; -2 0 4])  # avoid vector of row vector construction
⎡1   2  3⎤
⎢        ⎥
⎣-2  0  4⎦

julia> M.col_del(0)
⎡2  3⎤
⎢    ⎥
⎣0  4⎦

```

```jldoctest matrices
julia> M.row_del(1)
[1  2  3]

```


!!! note "Alert"
    For older versions of sympy, the following did not work (using symbolic  values as matrix entries without reverting to  their PyObjects  had shape issues);  this should work  now:


```jldoctest matrices
julia> @vars x
(x,)

julia> sympy.ImmutableMatrix([x 1;  1  x])
⎡x  1⎤
⎢    ⎥
⎣1  x⎦

```

----

!!! note "TODO"

    This is a mess. See issue 6992.

To insert rows or columns, use `row_insert` or `col_insert`.  These
operations **do not** operate in place.

```python
    >>> M
    [2  3]
    >>> M = M.row_insert(1, Matrix([[0, 4]]))
    >>> M
    ⎡2  3⎤
    ⎢    ⎥
    ⎣0  4⎦
    >>> M = M.col_insert(0, Matrix([1, -2]))
    >>> M
    ⎡1   2  3⎤
    ⎢        ⎥
    ⎣-2  0  4⎦
```

##### In `Julia`:

```jldoctest matrices
julia> M = sympy.ImmutableMatrix([2 3])
[2  3]

julia> M = M.row_insert(1, Sym[0 4])
⎡2  3⎤
⎢    ⎥
⎣0  4⎦

```

```jldoctest matrices
julia> M = M.col_insert(0, Sym[1, -2])
⎡1   2  3⎤
⎢        ⎥
⎣-2  0  4⎦

```

----

Unless explicitly stated, the methods mentioned below do not operate in
place. In general, a method that does not operate in place will return a new
`Matrix` and a method that does operate in place will return `None`.

##### In `Julia`

This would be the case for the immutable matrices.

----

## Basic Methods


As noted above, simple operations like addition and multiplication are done
just by using `+`, `*`, and `**`.  To find the inverse of a matrix, just
raise it to the `-1` power.

```python
    >>> M = Matrix([[1, 3], [-2, 3]])
    >>> N = Matrix([[0, 3], [0, 7]])
    >>> M + N
    ⎡1   6 ⎤
    ⎢      ⎥
    ⎣-2  10⎦
    >>> M*N
    ⎡0  24⎤
    ⎢     ⎥
    ⎣0  15⎦
    >>> 3*M
    ⎡3   9⎤
    ⎢     ⎥
    ⎣-6  9⎦
    >>> M**2
    ⎡-5  12⎤
    ⎢      ⎥
    ⎣-8  3 ⎦
    >>> M**-1
    ⎡1/3  -1/3⎤
    ⎢         ⎥
    ⎣2/9  1/9 ⎦
    >>> N**-1
    Traceback (most recent call last):
    ...
    ValueError: Matrix det == 0; not invertible.
```

##### In `Julia`:

In `Julia`,  we  use `M1` instead  of `N`, an exported symbol of `SymPy`. Otherise, it  all looks similar:

```jldoctest matrices
julia> M = Sym[1 3; -2 3]
2×2 Array{Sym,2}:
  1  3
 -2  3

julia> M1 = Sym[0 3; 0 7]
2×2 Array{Sym,2}:
 0  3
 0  7

julia> M + M1
2×2 Array{Sym,2}:
  1   6
 -2  10

julia> M*M1
2×2 Array{Sym,2}:
 0  24
 0  15

julia> 3*M
2×2 Array{Sym,2}:
  3  9
 -6  9

julia> M^2
2×2 Array{Sym,2}:
 -5  12
 -8   3

julia> M^-1
2×2 Array{Sym,2}:
 1/3  -1/3
 2/9   1/9
```

Attempting to find the inverse of  `M1` will  error (we suppress its lengthy output)

```jldoctest matrices
julia> using Test

julia> @test_throws  Exception M1^-1
Test Passed
      Thrown: PyCall.PyError

```

The above (except for the inverses) are using generic `Julia` definitions. For immutable matrices, we would have:



```jldoctest matrices
julia> M = sympy.ImmutableMatrix([1 3; -2 3])
⎡1   3⎤
⎢     ⎥
⎣-2  3⎦

julia> M1 = sympy.ImmutableMatrix([0 3; 0 7])
⎡0  3⎤
⎢    ⎥
⎣0  7⎦

julia> M + M1
⎡1   6 ⎤
⎢      ⎥
⎣-2  10⎦

```

```jldoctest matrices
julia> M*M1
⎡0  24⎤
⎢     ⎥
⎣0  15⎦

julia> 3*M
⎡3   9⎤
⎢     ⎥
⎣-6  9⎦

julia> M^2
         2
2

julia> M^-1
⎡1/3  -1/3⎤
⎢         ⎥
⎣2/9  1/9 ⎦
```

Similarly, `M1^(-1)` would yield an  error  for  the non-invertible matrix

* There is no broadcasting defined for the `SymMatrix` type.

-----

To take the transpose of a Matrix, use `T`.

```python
    >>> M = Matrix([[1, 2, 3], [4, 5, 6]])
    >>> M
    ⎡1  2  3⎤
    ⎢       ⎥
    ⎣4  5  6⎦
    >>> M.T
    ⎡1  4⎤
    ⎢    ⎥
    ⎢2  5⎥
    ⎢    ⎥
    ⎣3  6⎦
```

##### In `Julia`:

```jldoctest matrices
julia> M = Sym[1 2 3; 4 5 6]
2×3 Array{Sym,2}:
 1  2  3
 4  5  6

julia> M.T
3×2 Array{Sym,2}:
 1  4
 2  5
 3  6

```

----

## Matrix Constructors


Several constructors exist for creating common matrices.  To create an
identity matrix, use `eye`.  The command `eye(n)` will create an `n x n` identity matrix:

```python
    >>> eye(3)
    ⎡1  0  0⎤
    ⎢       ⎥
    ⎢0  1  0⎥
    ⎢       ⎥
    ⎣0  0  1⎦
    >>> eye(4)
    ⎡1  0  0  0⎤
    ⎢          ⎥
    ⎢0  1  0  0⎥
    ⎢          ⎥
    ⎢0  0  1  0⎥
    ⎢          ⎥
    ⎣0  0  0  1⎦
```

##### In `Julia`:

* `eye` is *not* exported so must qualified:

```jldoctest matrices
julia> sympy.eye(3)
3×3 Array{Sym,2}:
 1  0  0
 0  1  0
 0  0  1

julia> sympy.eye(4)
4×4 Array{Sym,2}:
 1  0  0  0
 0  1  0  0
 0  0  1  0
 0  0  0  1

```

----

To create a matrix of all zeros, use `zeros`.  `zeros(n, m)` creates an
`n x m` matrix of `0`s.

```python
    >>> zeros(2, 3)
    ⎡0  0  0⎤
    ⎢       ⎥
    ⎣0  0  0⎦
```

##### In `Julia`:

* zeros is extended but the method expects a symbolic first argument.  Either qualify it:

```jldoctest matrices
julia> sympy.zeros(2, 3)
2×3 Array{Sym,2}:
 0  0  0
 0  0  0

```

*or* create a symbolic first value:

```jldoctest matrices
julia> zeros(Sym(2), 3)
2×3 Array{Sym,2}:
 0  0  0
 0  0  0

```

*or* use the `Julia` constructor:

```jldoctest matrices
julia> zeros(Sym, 2, 3)
2×3 Array{Sym,2}:
 0  0  0
 0  0  0

```

----

Similarly, `ones` creates a matrix of ones.

```python
    >>> ones(3, 2)
    ⎡1  1⎤
    ⎢    ⎥
    ⎢1  1⎥
    ⎢    ⎥
    ⎣1  1⎦
```

##### In `Julia`:

* Similarly with `ones`:

```jldoctest matrices
julia> sympy.ones(3, 2)
3×2 Array{Sym,2}:
 1  1
 1  1
 1  1

```

----

To create diagonal matrices, use `diag`.  The arguments to `diag` can be
either numbers or matrices.  A number is interpreted as a `1 x 1`
matrix. The matrices are stacked diagonally.  The remaining elements are
filled with `0`\ s.

```python
    >>> diag(1, 2, 3)
    ⎡1  0  0⎤
    ⎢       ⎥
    ⎢0  2  0⎥
    ⎢       ⎥
    ⎣0  0  3⎦
    >>> diag(-1, ones(2, 2), Matrix([5, 7, 5]))
    ⎡-1  0  0  0⎤
    ⎢           ⎥
    ⎢0   1  1  0⎥
    ⎢           ⎥
    ⎢0   1  1  0⎥
    ⎢           ⎥
    ⎢0   0  0  5⎥
    ⎢           ⎥
    ⎢0   0  0  7⎥
    ⎢           ⎥
    ⎣0   0  0  5⎦
```

##### In `Julia`:

* similarly with `diag`:

```jldoctest matrices
julia> sympy.diag(1, 2, 3)
3×3 Array{Sym,2}:
 1  0  0
 0  2  0
 0  0  3

julia> sympy.diag(-1, sympy.ones(2, 2), sympy.Matrix([5, 7, 5]))
6×4 Array{Sym,2}:
 -1  0  0  0
  0  1  1  0
  0  1  1  0
  0  0  0  5
  0  0  0  7
  0  0  0  5

```

* The first one, could also use `Julia`'s `diagm` function from the `LinearAlgebra` package:

```jldoctest matrices
julia> diagm(0 => Sym[1,2,3])
3×3 Array{Sym,2}:
 1  0  0
 0  2  0
 0  0  3

```


----

## Advanced Methods


### Determinant


To compute the determinant of a matrix, use `det`.

```python
    >>> M = Matrix([[1, 0, 1], [2, -1, 3], [4, 3, 2]])
    >>> M
    ⎡1  0   1⎤
    ⎢        ⎥
    ⎢2  -1  3⎥
    ⎢        ⎥
    ⎣4  3   2⎦
    >>> M.det()
    -1
```

##### In `Julia`:

```jldoctest matrices
julia> M = Sym[1 0 1; 2 -1 3; 4 3 2]
3×3 Array{Sym,2}:
 1   0  1
 2  -1  3
 4   3  2

julia> M.det()
-1

```


Let

```jldoctest matrices
julia> @vars x
(x,)

julia> A = Sym[x 1; 1 x]
2×2 Array{Sym,2}:
 x  1
 1  x

```

The method for `det` falls back  the  sympy method:

```jldoctest matrices
julia> det(A)
 2
x  - 1

```

There is no reason  to,  but generic `Julia` methods  could be used:

```jldoctest matrices
julia> out  = lu(A)
LU{Sym,Array{Sym,2}}
L factor:
2×2 Array{Sym,2}:
 1  0
 x  1
U factor:
2×2 Array{Sym,2}:
 1        x
 0  1 - x^2

julia> prod(diag(out.L)) * prod(diag(out.U))
     2
1 - x 
```

### RREF


To put a matrix into reduced row echelon form, use `rref`.  `rref` returns
a tuple of two elements. The first is the reduced row echelon form, and the
second is a tuple of indices of the pivot columns.

```python
    >>> M = Matrix([[1, 0, 1, 3], [2, 3, 4, 7], [-1, -3, -3, -4]])
    >>> M
    ⎡1   0   1   3 ⎤
    ⎢              ⎥
    ⎢2   3   4   7 ⎥
    ⎢              ⎥
    ⎣-1  -3  -3  -4⎦
    >>> M.rref()
    ⎛⎡1  0   1    3 ⎤        ⎞
    ⎜⎢              ⎥        ⎟
    ⎜⎢0  1  2/3  1/3⎥, (0, 1)⎟
    ⎜⎢              ⎥        ⎟
    ⎝⎣0  0   0    0 ⎦        ⎠
```

##### In `Julia`:

```jldoctest matrices
julia> M = Sym[1 0 1 3; 2 3 4 7; -1 -3 -3 -4]
3×4 Array{Sym,2}:
  1   0   1   3
  2   3   4   7
 -1  -3  -3  -4

julia> M.rref()
(Sym[1 0 1 3; 0 1 2/3 1/3; 0 0 0 0], (0, 1))
```



!!! note

    The first element of the tuple returned by `rref` is of type
    `Matrix`. The second is of type `tuple`.

Nullspace
---------

To find the nullspace of a matrix, use `nullspace`. `nullspace` returns a
`list` of column vectors that span the nullspace of the matrix.

```python
    >>> M = Matrix([[1, 2, 3, 0, 0], [4, 10, 0, 0, 1]])
    >>> M
    ⎡1  2   3  0  0⎤
    ⎢              ⎥
    ⎣4  10  0  0  1⎦
    >>> M.nullspace()
    ⎡⎡-15⎤  ⎡0⎤  ⎡ 1  ⎤⎤
    ⎢⎢   ⎥  ⎢ ⎥  ⎢    ⎥⎥
    ⎢⎢ 6 ⎥  ⎢0⎥  ⎢-1/2⎥⎥
    ⎢⎢   ⎥  ⎢ ⎥  ⎢    ⎥⎥
    ⎢⎢ 1 ⎥, ⎢0⎥, ⎢ 0  ⎥⎥
    ⎢⎢   ⎥  ⎢ ⎥  ⎢    ⎥⎥
    ⎢⎢ 0 ⎥  ⎢1⎥  ⎢ 0  ⎥⎥
    ⎢⎢   ⎥  ⎢ ⎥  ⎢    ⎥⎥
    ⎣⎣ 0 ⎦  ⎣0⎦  ⎣ 1  ⎦⎦
```

##### In `Julia`:

* the list is mapped to an array of vectors, otherwise this is identical:

```jldoctest matrices
julia> M = Sym[1 2 3 0 0; 4 10 0 0 1]
2×5 Array{Sym,2}:
 1   2  3  0  0
 4  10  0  0  1

julia> M.nullspace()
3-element Array{Array{Sym,2},1}:
 [-15; 6; … ; 0; 0]
 [0; 0; … ; 1; 0]
 [1; -1/2; … ; 0; 1]

```



Columnspace
-----------

To find the columnspace of a matrix, use `columnspace`. `columnspace` returns a
`list` of column vectors that span the columnspace of the matrix.

```python
    >>> M = Matrix([[1, 1, 2], [2 ,1 , 3], [3 , 1, 4]])
    >>> M
    ⎡1  1  2⎤
    ⎢       ⎥
    ⎢2  1  3⎥
    ⎢       ⎥
    ⎣3  1  4⎦
    >>> M.columnspace()
    ⎡⎡1⎤  ⎡1⎤⎤
    ⎢⎢ ⎥  ⎢ ⎥⎥
    ⎢⎢2⎥, ⎢1⎥⎥
    ⎢⎢ ⎥  ⎢ ⎥⎥
    ⎣⎣3⎦  ⎣1⎦⎦
```

##### In `Julia`:

* as with `nullspace`, the return value is a vector of vectors:

```jldoctest matrices
julia> M = Sym[1 1 2; 2 1 3; 3 1 4]
3×3 Array{Sym,2}:
 1  1  2
 2  1  3
 3  1  4

julia> M.columnspace()
2-element Array{Array{Sym,2},1}:
 [1; 2; 3]
 [1; 1; 1]

```

----

Eigenvalues, Eigenvectors, and Diagonalization
----------------------------------------------

To find the eigenvalues of a matrix, use `eigenvals`.  `eigenvals`
returns a dictionary of `eigenvalue:algebraic multiplicity` pairs (similar to the
output of :ref:`roots <tutorial-roots>`).

```python
    >>> M = Matrix([[3, -2,  4, -2], [5,  3, -3, -2], [5, -2,  2, -2], [5, -2, -3,  3]])
    >>> M
    ⎡3  -2  4   -2⎤
    ⎢             ⎥
    ⎢5  3   -3  -2⎥
    ⎢             ⎥
    ⎢5  -2  2   -2⎥
    ⎢             ⎥
    ⎣5  -2  -3  3 ⎦
    >>> M.eigenvals()
    {-2: 1, 3: 1, 5: 2}
```

##### In `Julia`:

```jldoctest matrices
julia> M = Sym[3 -2  4 -2; 5  3 -3 -2; 5 -2  2 -2; 5 -2 -3  3]
4×4 Array{Sym,2}:
 3  -2   4  -2
 5   3  -3  -2
 5  -2   2  -2
 5  -2  -3   3

julia> M.eigenvals()
Dict{Any,Any} with 3 entries:
  3  => 1
  -2 => 1
  5  => 2

```

----

This means that `M` has eigenvalues -2, 3, and 5, and that the
eigenvalues -2 and 3 have algebraic multiplicity 1 and that the eigenvalue 5
has algebraic multiplicity 2.

To find the eigenvectors of a matrix, use `eigenvects`.  `eigenvects`
returns a list of tuples of the form `(eigenvalue:algebraic multiplicity,
[eigenvectors])`.

```python
    >>> M.eigenvects()
    ⎡⎛       ⎡⎡0⎤⎤⎞  ⎛      ⎡⎡1⎤⎤⎞  ⎛      ⎡⎡1⎤  ⎡0 ⎤⎤⎞⎤
    ⎢⎜       ⎢⎢ ⎥⎥⎟  ⎜      ⎢⎢ ⎥⎥⎟  ⎜      ⎢⎢ ⎥  ⎢  ⎥⎥⎟⎥
    ⎢⎜       ⎢⎢1⎥⎥⎟  ⎜      ⎢⎢1⎥⎥⎟  ⎜      ⎢⎢1⎥  ⎢-1⎥⎥⎟⎥
    ⎢⎜-2, 1, ⎢⎢ ⎥⎥⎟, ⎜3, 1, ⎢⎢ ⎥⎥⎟, ⎜5, 2, ⎢⎢ ⎥, ⎢  ⎥⎥⎟⎥
    ⎢⎜       ⎢⎢1⎥⎥⎟  ⎜      ⎢⎢1⎥⎥⎟  ⎜      ⎢⎢1⎥  ⎢0 ⎥⎥⎟⎥
    ⎢⎜       ⎢⎢ ⎥⎥⎟  ⎜      ⎢⎢ ⎥⎥⎟  ⎜      ⎢⎢ ⎥  ⎢  ⎥⎥⎟⎥
    ⎣⎝       ⎣⎣1⎦⎦⎠  ⎝      ⎣⎣1⎦⎦⎠  ⎝      ⎣⎣0⎦  ⎣1 ⎦⎦⎠⎦
```

##### In `Julia`:

* the output is less than desirable, as there is no special `show` method

* the `eigvals` and `eigvecs` methods present the output in the manner that `Julia`'s generic functions do:

```jldoctest matrices
julia> M.eigenvects()
3-element Array{Tuple{Sym,Int64,Array{Array{Sym,2},1}},1}:
 (-2, 1, [[0; 1; 1; 1]])
 (3, 1, [[1; 1; 1; 1]])
 (5, 2, [[1; 1; 1; 0], [0; -1; 0; 1]])

```

compare with

```jldoctest matrices
julia> eigvecs(M)
4×4 Array{Sym,2}:
 0  1  1   0
 1  1  1  -1
 1  1  1   0
 1  1  0   1

```



----

This shows us that, for example, the eigenvalue 5 also has geometric
multiplicity 2, because it has two eigenvectors.  Because the algebraic and
geometric multiplicities are the same for all the eigenvalues, `M` is
diagonalizable.

To diagonalize a matrix, use `diagonalize`. `diagonalize` returns a tuple
`(P, D)`, where `D` is diagonal and `M = PDP^{-1}`.

```python
    >>> P, D = M.diagonalize()
    >>> P
    ⎡0  1  1  0 ⎤
    ⎢           ⎥
    ⎢1  1  1  -1⎥
    ⎢           ⎥
    ⎢1  1  1  0 ⎥
    ⎢           ⎥
    ⎣1  1  0  1 ⎦
    >>> D
    ⎡-2  0  0  0⎤
    ⎢           ⎥
    ⎢0   3  0  0⎥
    ⎢           ⎥
    ⎢0   0  5  0⎥
    ⎢           ⎥
    ⎣0   0  0  5⎦
    >>> P*D*P**-1
    ⎡3  -2  4   -2⎤
    ⎢             ⎥
    ⎢5  3   -3  -2⎥
    ⎢             ⎥
    ⎢5  -2  2   -2⎥
    ⎢             ⎥
    ⎣5  -2  -3  3 ⎦
    >>> P*D*P**-1 == M
    True
```

##### In `Julia`:

```jldoctest matrices
julia> P, D = M.diagonalize()
(Sym[0 1 1 0; 1 1 1 -1; 1 1 1 0; 1 1 0 1], Sym[-2 0 0 0; 0 3 0 0; 0 0 5 0; 0 0 0 5])

julia> P
4×4 Array{Sym,2}:
 0  1  1   0
 1  1  1  -1
 1  1  1   0
 1  1  0   1

julia> D
4×4 Array{Sym,2}:
 -2  0  0  0
  0  3  0  0
  0  0  5  0
  0  0  0  5

julia> P*D*P^-1
4×4 Array{Sym,2}:
 3  -2   4  -2
 5   3  -3  -2
 5  -2   2  -2
 5  -2  -3   3

julia> P*D*P^-1 == M
true

```



!!! note "Quick Tip"
   `lambda` is a reserved keyword in Python, so to create a Symbol called $\lambda$, while using the same names for SymPy Symbols and Python variables, use `lamda` (without the `b`).  It will still pretty print as $\lambda$.

Note that since `eigenvects` also includes the eigenvalues, you should use
it instead of `eigenvals` if you also want the eigenvectors. However, as
computing the eigenvectors may often be costly, `eigenvals` should be
preferred if you only wish to find the eigenvalues.

If all you want is the characteristic polynomial, use `charpoly`.  This is
more efficient than `eigenvals`, because sometimes symbolic roots can be
expensive to calculate.

```python
    >>> lamda = symbols('lamda')
    >>> p = M.charpoly(lamda)
    >>> factor(p)
           2
    (λ - 5) ⋅(λ - 3)⋅(λ + 2)
```

##### In `Julia`:

* note missing `b` is not needed with `Julia`:

```jldoctest matrices
julia> lambda = symbols("lambda")
λ

julia> p = M.charpoly(lambda)
PurePoly(lambda**4 - 11*lambda**3 + 29*lambda**2 + 35*lambda - 150, lambda, domain='ZZ')

julia> factor(p) |>  string
"(lambda - 5)^2*(lambda - 3)*(lambda + 2)"

```

----

!!! note "TODO"

    Add an example for `jordan_form`, once it is fully implemented.

## Possible Issues


Zero Testing
------------

If your matrix operations are failing or returning wrong answers,
the common reasons would likely be from zero testing.
If there is an expression not properly zero-tested,
it can possibly bring issues in finding pivots for gaussian elimination,
or deciding whether the matrix is inversible,
or any high level functions which relies on the prior procedures.

Currently, the SymPy's default method of zero testing `_iszero` is only
guaranteed to be accurate in some limited domain of numerics and symbols,
and any complicated expressions beyond its decidability are treated as `None`,
which behaves similarly to logical `False`.

The list of methods using zero testing procedures are as followings.

`echelon_form` , `is_echelon` , `rank` , `rref` , `nullspace` ,
`eigenvects` , `inverse_ADJ` , `inverse_GE` , `inverse_LU` ,
`LUdecomposition` , `LUdecomposition_Simple` , `LUsolve`

They have property `iszerofunc` opened up for user to specify zero testing
method, which can accept any function with single input and boolean output,
while being defaulted with `_iszero`.

Here is an example of solving an issue caused by undertested zero.
[#zerotestexampleidea-fn]_ [#zerotestexamplediscovery-fn]_

```python
    >>> from sympy import *
    >>> q = Symbol("q", positive = True)
    >>> m = Matrix([
    ... [-2*cosh(q/3),      exp(-q),            1],
    ... [      exp(q), -2*cosh(q/3),            1],
    ... [           1,            1, -2*cosh(q/3)]])
    >>> m.nullspace()
    []
```

##### In `Julia`:

```jldoctest matrices
julia> q = sympy.Symbol("q", positive = true)
q

julia> m = Sym[-2*cosh(q/3) exp(-q) 1; exp(q) -2*cosh(q/3) 1; 1 1 -2*cosh(q/3)]
3×3 Array{Sym,2}:
 -2*cosh(q/3)       exp(-q)             1
       exp(q)  -2*cosh(q/3)             1
            1             1  -2*cosh(q/3) 

julia> m.nullspace()
0-element Array{Any,1}

```

----

You can trace down which expression is being underevaluated,
by injecting a custom zero test with warnings enabled.

```python
    >>> import warnings
    >>>
    >>> def my_iszero(x):
    ...     try:
    ...         result = x.is_zero
    ...     except AttributeError:
    ...         result = None
    ...
    ...     # Warnings if evaluated into None
    ...     if result == None:
    ...         warnings.warn("Zero testing of {} evaluated into {}".format(x, result))
    ...     return result
    ...
    >>> m.nullspace(iszerofunc=my_iszero) # doctest: +SKIP
    __main__:9: UserWarning: Zero testing of 4*cosh(q/3)**2 - 1 evaluated into None
    __main__:9: UserWarning: Zero testing of (-exp(q) - 2*cosh(q/3))*(-2*cosh(q/3) - exp(-q)) - (4*cosh(q/3)**2 - 1)**2 evaluated into None
    __main__:9: UserWarning: Zero testing of 2*exp(q)*cosh(q/3) - 16*cosh(q/3)**4 + 12*cosh(q/3)**2 + 2*exp(-q)*cosh(q/3) evaluated into None
    __main__:9: UserWarning: Zero testing of -(4*cosh(q/3)**2 - 1)*exp(-q) - 2*cosh(q/3) - exp(-q) evaluated into None
    []
```

##### In `Julia`:

Is this available??

----


In this case,
`(-exp(q) - 2*cosh(q/3))*(-2*cosh(q/3) - exp(-q)) - (4*cosh(q/3)**2 - 1)**2`
should yield zero, but the zero testing had failed to catch.
possibly meaning that a stronger zero test should be introduced.
For this specific example, rewriting to exponentials and applying simplify would
make zero test stronger for hyperbolics,
while being harmless to other polynomials or transcendental functions.

```python
    >>> def my_iszero(x):
    ...     try:
    ...         result = x.rewrite(exp).simplify().is_zero
    ...     except AttributeError:
    ...         result = None
    ...
    ...     # Warnings if evaluated into None
    ...     if result == None:
    ...         warnings.warn("Zero testing of {} evaluated into {}".format(x, result))
    ...     return result
    ...
    >>> m.nullspace(iszerofunc=my_iszero) # doctest: +SKIP
    __main__:9: UserWarning: Zero testing of -2*cosh(q/3) - exp(-q) evaluated into None
    ⎡⎡  ⎛   q         ⎛q⎞⎞  -q         2⎛q⎞    ⎤⎤
    ⎢⎢- ⎜- ℯ  - 2⋅cosh⎜─⎟⎟⋅ℯ   + 4⋅cosh ⎜─⎟ - 1⎥⎥
    ⎢⎢  ⎝             ⎝3⎠⎠              ⎝3⎠    ⎥⎥
    ⎢⎢─────────────────────────────────────────⎥⎥
    ⎢⎢          ⎛      2⎛q⎞    ⎞     ⎛q⎞       ⎥⎥
    ⎢⎢        2⋅⎜4⋅cosh ⎜─⎟ - 1⎟⋅cosh⎜─⎟       ⎥⎥
    ⎢⎢          ⎝       ⎝3⎠    ⎠     ⎝3⎠       ⎥⎥
    ⎢⎢                                         ⎥⎥
    ⎢⎢           ⎛   q         ⎛q⎞⎞            ⎥⎥
    ⎢⎢          -⎜- ℯ  - 2⋅cosh⎜─⎟⎟            ⎥⎥
    ⎢⎢           ⎝             ⎝3⎠⎠            ⎥⎥
    ⎢⎢          ────────────────────           ⎥⎥
    ⎢⎢                   2⎛q⎞                  ⎥⎥
    ⎢⎢             4⋅cosh ⎜─⎟ - 1              ⎥⎥
    ⎢⎢                    ⎝3⎠                  ⎥⎥
    ⎢⎢                                         ⎥⎥
    ⎣⎣                    1                    ⎦⎦
```

##### In `Julia`:

Is this available?


----


You can clearly see `nullspace` returning proper result, after injecting an
alternative zero test.

Note that this approach is only valid for some limited cases of matrices
containing only numerics, hyperbolics, and exponentials.
For other matrices, you should use different method opted for their domains.

Possible suggestions would be either taking advantage of rewriting and
simplifying, with tradeoff of speed [#zerotestsimplifysolution-fn]_ ,
or using random numeric testing, with tradeoff of accuracy
[#zerotestnumerictestsolution-fn]_ .

If you wonder why there is no generic algorithm for zero testing that can work
with any symbolic entities,
it's because of the constant problem stating that zero testing is undecidable
[#constantproblemwikilink-fn]_ ,
and not only the SymPy, but also other computer algebra systems
[#mathematicazero-fn]_ [#matlabzero-fn]_
would face the same fundamental issue.

However, discovery of any zero test failings can provide some good examples to
improve SymPy,
so if you have encountered one, you can report the issue to
SymPy issue tracker [#sympyissues-fn]_ to get detailed help from the community.

!!! note "Footnotes"

    * [#zerotestexampleidea-fn] Inspired by https://gitter.im/sympy/sympy?at=5b7c3e8ee5b40332abdb206c
    * [#zerotestexamplediscovery-fn] Discovered from https://github.com/sympy/sympy/issues/15141
    * [#zerotestsimplifysolution-fn] Suggested from https://github.com/sympy/sympy/issues/10120
    * [#zerotestnumerictestsolution-fn] Suggested from https://github.com/sympy/sympy/issues/10279
    * [#constantproblemwikilink-fn] https://en.wikipedia.org/wiki/Constant_problem
    * [#mathematicazero-fn] How mathematica tests zero https://reference.wolfram.com/language/ref/PossibleZeroQ.html
    * [#matlabzero-fn] How matlab tests zero https://www.mathworks.com/help/symbolic/mupad_ref/iszero.html
	* [#sympyissues-fn] https://github.com/sympy/sympy/issues

