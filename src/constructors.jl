##################################################
function symbols(x::AbstractString; kwargs...)
    out = sympy.symbols(x; kwargs...)
end
symbols(x::Symbol; kwargs...) = symbols(string(x); kwargs...)


"""
    @vars x y z

The `vars` macro is identical to  `@syms`.

"""
macro vars(x...)
    q = Expr(:block)
    as = []    # running list of assumptions to be applied
    ss = []    # running list of symbols created
    for s in reverse(x)
        if isa(s, Expr)    # either an assumption or a named variable
            if s.head == :(=)
                s.head = :kw
                push!(as, s)
            elseif s.head == :(=>)
                push!(ss, s.args[1])
                push!(q.args, Expr(:(=), esc(s.args[1]), Expr(:call, :symbols, s.args[2], map(esc,as)...)))
            end
        elseif isa(s, Symbol)   # raw symbol to be created
            push!(ss, s)
            push!(q.args, Expr(:(=), esc(s), Expr(:call, :symbols, Expr(:quote, s), map(esc,as)...)))
        else
            throw(AssertionError("@syms expected a list of symbols and assumptions"))
        end
    end
    push!(q.args, Expr(:tuple, map(esc,reverse(ss))...)) # return all of the symbols we created
    q
end

Sym(s::SymbolicObject) = s
Sym(s::AbstractString) = sympy.sympify(s)
convert(::Type{Sym}, s::AbstractString) = Sym(s)

sympify(s, args...; kwargs...) = sympy.sympify(s, args...; kwargs...)



SymMatrix(s::SymMatrix) = s
SymMatrix(s::Sym) = sympy.Matrix([s])
SymMatrix(A::Matrix) = sympy.Matrix([A[i,:] for i in 1:size(A)[1]])
