##################################################
"""
    symbols(name(s), assumptions...)

Calls `sympy.symbols` to produce symbolic variables and symbolic functions. An alternate to the recommended `@syms`, (when applicable)

In sympy `sympy.symbols` and `sympy.Symbol` both allow the construction of symbolic variables and functions. The `Julia` function `symbols` is an alias for `sympy.symbols`.

* Variables are created through `x=symbols("x")`;
* Assumptions on variables by `x=symbols("x", real=true)`;
* Multiple symbols `x1,x2 = symbols("x[1:3]")` can be created. Unlike `@syms`, the number of variables can be specified with a variable through interpolation.
* Symbolic functions can be created py passing `cls=sympy.Function`, `symbols("F", cls=sympy.Function, real=true)`

"""
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
!!! Note:
    The `@syms` macro is recommended as it has a more flexible syntax
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

"""
    @syms a n::integer x::(real,positive)=>"x₀" y[-1:1] u() v()::real w()::(real,positive) y()[1:3]::real

Construct symbolic variables or functions along with specified assumptions. Similar to `@vars`, `sympy.symbols`, and `sympy.Function`, but the specification of the assumptions is more immediate than those interfaces which follow sympy's constructors.

Allows the specification of assumptions on the variables and functions.

* a type-like annontation, such as `n::integer` is equivalent to `sympy.symbols("n", integer=true)`. Multiple assumptions are combined using parentheses (e.g., `n::(integer,nonnegative)`.

The possible [values](https://docs.sympy.org/latest/modules/core.html#module-sympy.core.assumptions) for assumptions are: "commutative", "complex", "imaginary", "real", "integer", "odd", "even", "prime", "composite", "zero", "nonzero", "rational", "algebraic", "transcendental", "irrational", "finite", "infinite", "negative", "nonnegative", "positive", "nonpositive", "hermitian", "antihermetian".

* a tensor declaration form is provided to define arrays of variables, e.g. `x[-1:1]` or `y[1:4, 2:5]`.

* a symbolic function can be specified using a pair of parentheses after the name, as in `u()`.

* The return type of a function can have assumptions specified, as with a variable. E.g., `h()::complex`. How the symbolic function prints can be set as with a variable, e.g. `h()::complex=>"h̄"`.

* multiple definitions can be separated by commas

* How the symbol prints (the `__str__()` value) can be specified using the syntax `=>"name"`, as in `x=>"xₒ"`

## Examples:

```jldoctest constructors
julia> using SymPy

julia> @syms a b::nonnegative
(a, b)

julia> sqrt(a^2), sqrt(b^2)
(sqrt(a^2), b)
```

```jldoctest constructors
julia> @syms x::prime
(x,)

julia> ask(𝑄.negative(x)), ask(𝑄.integer(x)), ask(𝑄.even(x))  # (false, true, nothing)
(false, true, nothing)
```

```jldoctest constructors
julia> @syms a[0:5], x
(Sym[a₀, a₁, a₂, a₃, a₄, a₅], x)

julia> sum( aᵢ*x^(i) for (i,aᵢ) ∈ zip(0:5, a)) |> print
a₀ + a₁*x + a₂*x^2 + a₃*x^3 + a₄*x^4 + a₅*x^5
```


```jldoctest constructors
julia> @syms x u() v()::nonnegative
(x, u, v)

julia> sqrt(u(x)^2), sqrt(v(x)^2) # sqrt(u(x)^2), Abs(v(x))
(sqrt(u(x)^2), Abs(v(x)))
```

!!! Note:
    Many thanks to `@matthieubulte` for this contribution.
"""
macro syms(xs...)
    # If the user separates declaration with commas, the top-level expression is a tuple
    if length(xs) == 1 && isa(xs[1], Expr) && xs[1].head == :tuple
        _gensyms(xs[1].args...)
    elseif length(xs) > 0
        _gensyms(xs...)
    end
end

function _gensyms(xs...)
    asstokw(a) = Expr(:kw, esc(a), true)

    # Each declaration is parsed and generates a declaration using `symbols`
    symdefs = map(xs) do expr
        decl = parsedecl(expr)
        symname = sym(decl)
        symname, gendecl(decl)
    end
    syms, defs = collect(zip(symdefs...))

    # The macro returns a tuple of Symbols that were declared
    Expr(:block, defs..., :(tuple($(map(esc,syms)...))))
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
