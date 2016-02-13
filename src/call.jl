## Call symbolic object with natural syntax
function (ex::SymbolicObject)(arg...)
    xs = free_symbols(ex)
    subs(ex, collect(zip(xs, args))...)
end
(ex::SymbolicObject)(x::Dict) = subs(ex, x)
(ex::SymbolicObject)(x::Pair...) = subs(ex, x...)
