function Base.call{T <: SymbolicObject}(ex::T; kwargs...)
    warn("""
Calling an expression with keyword arguments will be deprecated. From v0.4 onward, the use of pairs, as in
var1=>val1, var2=>val2)` is suggested.
""")
    subs(ex, kwargs...)
end

function Base.call{T <: SymbolicObject}(ex::T, args...)
    xs = free_symbols(ex)
    subs(ex, collect(zip(xs, args))...)
end
Base.call(ex::SymbolicObject, x::Dict) = subs(ex, x)
Base.call(ex::SymbolicObject, x::Pair...) = subs(ex, x...)

## for symbolic functinos (dsolve)
Base.call(u::SymFunction, x::Base.Dict) = throw(ArgumentError("IVPsolutions can only be called with symbolic objects"))
Base.call(u::SymFunction, x::Base.Pair) = throw(ArgumentError("IVPsolutions can only be called with symbolic objects"))
function Base.call(u::SymFunction, x) 
    if u.n == 0
        u.u(SymPy.project(x))
    else
        __x = Sym("__x")
        diff(u.u(__x.x), __x, u.n)(__x => x)
    end
end
