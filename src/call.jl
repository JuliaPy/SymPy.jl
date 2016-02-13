## Call symbolic object with natural syntax
## ex(x=>val)
function (ex::T){T<:SymbolicObject}(args...)
    xs = free_symbols(ex)
    subs(ex, collect(zip(xs, args))...)
end
(ex::SymbolicObject)(x::Dict) = subs(ex, x)
(ex::SymbolicObject)(x::Pair...) = subs(ex, x...)
