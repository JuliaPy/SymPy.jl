## Call symbolic object with natural syntax
## ex(x=>val)
## how to do from any symbolic object?
function (ex::Sym)(args...)
    xs = free_symbols(ex)
    subs(ex, collect(zip(xs, args))...)
end
(ex::Sym)(x::Dict) = subs(ex, x)
(ex::Sym)(x::Pair...) = subs(ex, x...)


## for symbolic functions (dsolve.jl)
(u::SymFunction)(x::Base.Dict) = throw(ArgumentError("IVPsolutions can only be called with symbolic objects"))
(u::SymFunction)(x::Base.Pair) = throw(ArgumentError("IVPsolutions can only be called with symbolic objects"))
function (u::SymFunction)(x) 
    if u.n == 0
        u.u(SymPy.project(x))
    else
        __x = Sym("__x")
        diff(u.u(__x.x), __x, u.n)(__x => x)
    end
end
