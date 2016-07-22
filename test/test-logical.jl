using SymPy
using Base.Test


## More logical expressions
@vars x y
(x ≪ 0) ∧ (x*y ≤ 1) ∨ (x ⩵ y) ∧ (¬(x ≫ 3))
