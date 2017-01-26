using SymPy
if VERSION >= v"0.5.0-dev+7720"
    using Base.Test
else
    using BaseTestNext
    const Test = BaseTestNext
end


@testset "Logical" begin
    ## More logical expressions
    @vars x y
    (x ≪ 0) ∧ Le(x*y,1) ∨ (x ⩵ y) ∧ (¬(x ≫ 3))
end
