using SymPy
using Base.Test


## More logical expressions
@vars x y
(x ≪ 0) ∧ Le(x*y,1) ∨ (x ⩵ y) ∧ (¬(x ≫ 3))
