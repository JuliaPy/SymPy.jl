## Call symbolic object with natural syntax
## ex(x=>val)
## how to do from any symbolic object?
function (ex::Sym)(args...)
    xs = free_symbols(ex)
    subs(ex, collect(zip(xs, args))...)
end
(ex::Sym)(x::Dict) = subs(ex, x)
(ex::Sym)(x::Pair...) = subs(ex, x...)
