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
