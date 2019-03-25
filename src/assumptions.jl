## Assumptions
## http://docs.sympy.org/0.7.2/modules/assumptions/index.html

"""
refine: http://docs.sympy.org/dev/modules/assumptions/refine.html
"""
refine(ex, assumpts...) = sympy.refine(ex, assumpts...)
export refine

"""

`ask`. Returns true, false or nothing

Example:

```
ask(Q.integer(x*y), And(Q.integer(x), Q.integer(y)))
## really slow isprime:
filter(x -> ask(Q.prime(x)), [1:1000])
```

"""
ask(x::Sym, args...) = sympy.ask(x, args...)
ask(x::Bool, args...) = x
ask(x::Nothing, args...) = x
export ask

## should we support & and | for (sym,sym) pairs? Not sure
## dependso on what happens to | (x==0) and ex == x usage for ==
## for now, we can combine terms logically with And, Or, Not...
## these are in logic module


## simple methods (x, args) -> y (y coercion happens via PyCall)
logic_sympy_methods = (
                     :And, :Or, :Not,
                     :Xor, :Nand, :Nor, :Implies,
                     :Equivalent,
                     :satisfiable
                     )


## We make a module Q to hold the assumptions
## this follows this page http://docs.sympy.org/0.7.5/_modules/sympy/assumptions/ask.html
"""

Documentation for the `Q` module.

SymPy allows for
[assumptions](http://docs.sympy.org/latest/modules/sympy/assumptions/)
on variables. These may be placed on free sympols at construction.

For example, the following creates a real value variable `x` and a postive, real variable `y`:

```
@vars x real=true
@vars y real=true positive=true
```

The `Q` module exposes a means to *q*uery the assumptions on a
variable. For example,

```
ask(Q.positive(y))  # true
ask(Q.negative(y))  # false
ask(Q.positive(x))  # `nothing`
ask(Q.positive(x^2)) # `nothing` -- might be 0
ask(Q.positive(1 + x^2) # true  -- must be postive now.
```

The ask function uses tri-state logic, returning one of 3 values:
`true`; `false`; or `nothing`, when the query is indeterminate.

The construction of predicates is done through `Q` methods. These can
be combined logically. For example, this will be `true`:

```
ask(Q.positive(y) & Q.negative(-x^2 - 1))
```

The above use `&` as an infix operation for the binary operator
`And`. Values can also be combined with `Or`, `Not`, `Xor`, `Nand`,
`Nor`, `Implies`, `Equivalent`, and `satisfiable`.

Note: As `SymPy.jl` converts symbolic matrices into Julia's `Array`
type and not as matrices within Python, the predicate functions from SymPy for
matrices are not used, though a replacement is given.
"""
module Q
import SymPy
import PyCall
import LinearAlgebra: det, norm

##http://docs.sympy.org/dev/_modules/sympy/assumptions/ask.html#ask
Q_predicates = (:antihermitian,
                :bounded, :finite, # bounded deprecated
                :commutative,
                :complex,
                :composite,
                :even,
                :extended_real,
                :hermitian,
                :imaginary,
                :infinitesimal,
                :infinity, :infinite, # :infinity deprecated
                :integer,
                :irrational,
                :rational,
                :algebraic,
                :transcendental,
                :negative,
                :nonzero, :zero,
                :positive,
                :prime,
                :real,
                :odd,
                :is_true,
                :nonpositive,
                :nonnegative
#                :symmetric,
#                :invertible,
#                :singular,
#                :orthogonal,
#                :unitary,
#                :normal,
#                :positive_definite,
#                :upper_triangular,
#                :lower_triangular,
#                :diagonal,
#                :triangular,
#                :unit_triangular,
#                :fullrank,
#                :square,
#                :real_elements,
#                :complex_elements,
#                :integer_elements
)

for meth in Q_predicates
   nm = string(meth)
      @eval begin
#             @doc """
# `$($nm)`: a SymPy function.
# The SymPy documentation can be found through: http://docs.sympy.org/latest/search.html?q=$($nm)
# """ ->
            ($meth)(x) = PyCall.pycall(SymPy.sympy.Q.$nm, SymPy.Sym, x)::SymPy.Sym
   end
end


symmetric(M::Array{T,2}) where {T <: SymPy.Sym} = SymPy.issymmetric(M)
function invertible(M::Array{T,2}) where {T <: SymPy.Sym}
    d = det(M)
    pos = SymPy.ask(positive(d))
    if pos == nothing
        return nothing
    elseif pos == true
        return true
    end
    neg = SymPy.ask(negative(d))
    if neg == nothing
        return nothing
    end
    z = SymPy.ask(zero(d))
    if z == nothing
        return nothing
    elseif z == true
        return false
    end

    return true
end

function singular(M::Array{T,2}) where {T <: SymPy.Sym}
    !invertible(M)
end

function orthogonal(M::Array{T,2}) where {T <: SymPy.Sym}
    vals = SymPy.simplify.(SymPy.simplify.(M*transpose(M)) .== one(T))
    no_nothing = 0
    for val in vals
        a = SymPy.ask(zero(val))
        if a == nothing
            no_nothing += 1
        elseif a == false
            return false
        end
    end

    no_nothing > 0 && return nothing
    return true
end


function unitary(M::Array{T,2}) where {T <: SymPy.Sym}
    vals = SymPy.simplify.(SymPy.simplify.(M*ctranspose(M)) .== one(T))
    no_nothing = 0
    for val in vals
        a = SymPy.ask(zero(val))
        if a == nothing
            no_nothing += 1
        elseif a == false
            return false
        end
    end

    no_nothing > 0 && return nothing
    return true
end

function normal(M::Array{T,2}) where {T <: SymPy.Sym}
    lhs = ctranspose(M) * M
    rhs = M * ctranspose(M)
    vals = zero.(SymPy.simplify.(lhs - rhs))
    no_nothing = 0
    for val in vals
        a = SymPy.ask(val)
        if a == nothing
            no_nothing += 1
        elseif a == false
            return false
        end
    end

    no_nothing > 0 && return nothing
    return true
end

# Use [Sylvester's](https://en.wikipedia.org/wiki/Sylvester%27s_criterion) Criteria
function positive_definite(M::Array{T,2}) where {T <: SymPy.Sym}
    !SymPy.ask(square(M)) && return false
    !SymPy.ask(symmetric(M))  && return false
    m, n = size(M)
    no_false = 0
    no_nothing = 0
    for i in 1:m
        a = SymPy.ask(Q.positive(det(M[1:i, 1:i])))
        if a == nothing no_nothing += 1 end
        if a == false no_false += 1 end
    end
    if no_false > 0
        return false
    elseif no_nothing > 0
        return nothing
    else
        return true
    end
end



upper_triangular(M::Array{T,2}) where {T <: SymPy.Sym} = SymPy.istriu(M)
lower_triangular(M::Array{T,2}) where {T <: SymPy.Sym} = SymPy.istril(M)
diagonal(M::Array{T,2}) where {T <: SymPy.Sym} = upper_triangular(M) && lower_triangular(M)
triangular(M::Array{T,2}) where {T <: SymPy.Sym} = upper_triangular(M) || lower_triangular(M)

## This is likely not the best way as it is a bit fidgety due
## to the call to rref.
function full_rank(M::Array{T,2}) where {T <: SymPy.Sym}
    m,n = size(M)
    m <= n || return full_rank(transpose(M))


    rr, p = SymPy.rref(M)
    lr = rr[end, :] # is this zero?
    no_nothing = 0
    no_nonzero = 0
    for val in lr
        a = SymPy.ask(nonzero(val))
        if a == nothing
            no_nothing += 1
        end
        if a == true
            no_nonzero += 1
        end
    end
    if no_nothing > 0
        return nothing
    elseif no_nonzero == 0
        return false
    else
        return true
    end

end


function square(M::Array{T,2}) where {T <: SymPy.Sym}
    m,n = SymPy.size(M)
    m == n
end


function real_elements(M::Array{T,2}) where {T <: SymPy.Sym}
    vals = real.(M)
    for val in vals
        a = SymPy.ask(real(val))
        (a == nothing || a == false) && return false
    end
    return true
end


function complex_elements(M::Array{T,2}) where {T <: SymPy.Sym}
    vals = real.(M)
    for val in vals
        a = SymPy.ask(SymPy.sympy."Q".complex(val))
        (a == nothing || a == false) && return false
    end
    return true

end


function integer_elements(M::Array{T,2}) where {T <: SymPy.Sym}
    vals = real.(M)
    for val in vals
        a = SymPy.ask(integer(val))
        (a == nothing || a == false) && return false
    end
    return true
end





end

## export
export Q
