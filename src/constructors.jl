##################################################
function symbols(x::AbstractString; kwargs...)
    out = sympy.symbols(x; kwargs...)
end
symbols(x::Symbol; kwargs...) = symbols(string(x); kwargs...)
symbols(xs::T...; kwargs...) where {T <: SymbolicObject} = xs

"""
    @vars x y z

Define symbolic values, possibly with names and assumptions

Examples:
```
@vars x y
@vars a1=>"α₁"
@vars a b real=true
```

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
            elseif s.head == :call && s.args[1] == :(=>)
                push!(ss, s.args[2])
                push!(q.args, Expr(:(=), esc(s.args[2]), Expr(:call, :symbols, s.args[3], map(esc,as)...)))
            end
        elseif isa(s, Symbol)   # raw symbol to be created
            push!(ss, s)
            push!(q.args, Expr(:(=), esc(s), Expr(:call, :symbols, Expr(:quote, s), map(esc,as)...)))
        else
            throw(AssertionError("@vars expected a list of symbols and assumptions"))
        end
    end
    push!(q.args, Expr(:tuple, map(esc,reverse(ss))...)) # return all of the symbols we created
    q
end


macro syms(xs...)
    # If the user separates declaration with commas, the top-level expression is a tuple
    if length(xs) == 1 && xs[1].head == :tuple
        return :(@syms($(xs[1].args...)))
    elseif length(xs) == 0
        return nothing
    end

    asstokw(a) = Expr(:kw, esc(a), true)
    
    # Each declaration is parsed and generates a declaration using `symbols`
    symdefs = map(xs) do expr
        varname, sym, assumptions, isfun = parsedecl(expr)
        ctor = isfun ? :SymFunction : :symbols
        sym, :($(esc(sym)) = $(esc(ctor))($(varname), $(map(asstokw, assumptions)...)))
    end
    syms, defs = collect(zip(symdefs...))

    # The macro returns a tuple of Symbols that were declared
    Expr(:block, defs..., :(tuple($(map(esc,syms)...))))
end

function parsedecl(expr)
    # @vars x
    if isa(expr, Symbol)
        return String(expr), expr, [], false
    # @vars x::assumptions, where assumption = assumptionkw | (assumptionkw...)
    elseif isa(expr, Expr) && expr.head == :(::)
        symexpr, assumptions = expr.args
        _, sym, _, isfun = parsedecl(symexpr)
        assumptions = isa(assumptions, Symbol) ? (assumptions,) : assumptions.args
        return String(sym), sym, assumptions, isfun
    # @vars x=>"name" 
    elseif isa(expr, Expr) && expr.head == :call && expr.args[1] == :(=>)
        length(expr.args) == 3 || parseerror()
        isa(expr.args[3], String) || parseerror()

        expr, strname = expr.args[2:end]
        _, sym, assumptions, isfun = parsedecl(expr)
        return strname, sym, assumptions, isfun
    # @vars x()
    elseif isa(expr, Expr) && expr.head == :call && expr.args[1] != :(=>)
        length(expr.args) == 1 || parseerror()
        isa(expr.args[1], Symbol) || parseerror()

        sym = expr.args[1]
        return String(sym), sym, [], true
    else
        parseerror()
    end
end

function parseerror()
    error("Incorrect @syms syntax. Try `@syms x::(real,positive)=>\"x₀\" y() z::complex n::integer` for instance.")
end

## avoid PyObject conversion as possible
Sym(x::T) where {T <: Number} = sympify(x)
Sym(x::Rational{T}) where {T} = Sym(numerator(x))/Sym(denominator(x))
function Sym(x::Complex{Bool})
    !x.re && x.im && return IM
    !x.re && !x.im && return zero(Sym)
    x.re && !x.im && return Sym(1)
    x.re && x.im && return Sym(1) + IM
end
Sym(x::Complex{T}) where {T} = Sym(real(x)) + Sym(imag(x)) * IM
Sym(xs::Symbol...) = Tuple(Sym.((string(x) for x in xs)))
Sym(x::AbstractString) = sympy.symbols(x)
Sym(s::SymbolicObject) = s
Sym(x::Irrational{T}) where {T} = convert(Sym, x)

convert(::Type{Sym}, s::AbstractString) = Sym(s)

sympify(s, args...; kwargs...) = pycall(sympy.sympify::PyCall.PyObject, Sym, s) #sympy.sympify(s, args...; kwargs...)



SymMatrix(s::SymMatrix) = s
SymMatrix(s::Sym) = sympy.ImmutableMatrix([s])
SymMatrix(A::Matrix) = sympy.ImmutableMatrix([A[i,:] for i in 1:size(A)[1]])
