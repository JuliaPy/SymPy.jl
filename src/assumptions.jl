## Assumptions
## http://docs.sympy.org/0.7.2/modules/assumptions/index.html

## TODO: Deprecate Qeven, etc. in favor of Q.even


"""
refine: http://docs.sympy.org/dev/modules/assumptions/refine.html
"""
refine(ex, assumpts...) = sympy_meth(:refine, ex, assumpts...)
export refine

"""

ask. Returns true, false or nothing

ask(Qinteger(x*y), And(Qinteger(x), Qinteger(y)))
## really slow isprime:
filter(x -> ask(Qprime(x)), [1:1000])

"""
ask(x::Sym, args...) = sympy_meth(:ask, x, args...)
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


## This is a bit messed up, as we use Qeven in place of Q.even, ...
## This is now deprecated. See below
Q_predicates = (:even, :odd, :prime, :nonzero,
                :complex, :extended_real, :imaginary, :infinitesimal,
                :integer, :irrational, :real,
                :positive, :negative,
                :bounded, :commutative)
Q_nms = [symbol("Q" * string(i)) for i in Q_predicates]

for (fn, meth) in zip(Q_nms, Q_predicates)
    nm = string(meth)
    @eval begin
        @doc """
`$($nm)`: a SymPy function.
The SymPy documentation can be found through: http://docs.sympy.org/latest/search.html?q=$($nm)
""" ->
        ($fn)(x::SymbolicObject) = PyCall.pyeval("f(x)", f=SymPy.sympy[:Q][($nm)], x=SymPy.project(x))
    end
    eval(Expr(:export, fn))
end

@deprecate Qeven(x) Q.even(x)
@deprecate Qodd(x) Q.odd(x)
@deprecate Qprime(x) Q.prime(x)
@deprecate Qnonzero(x) Q.nonzero(x)
@deprecate Qcomplex(x) Q.complex(x)
@deprecate Qextended_real(x) Q.extended_real(x)
@deprecate Qimaginary(x) Q.imaginary(x)
@deprecate Qinfinitesimal(x) Q.infinitesimal(x)
@deprecate Qinteger(x) Q.integer(x)
@deprecate Qirrational(x) Q.irrational(x)
@deprecate Qreal(x) Q.real(x)
@deprecate Qpositive(x) Q.positive(x)
@deprecate Qnegative(x) Q.negative(x)
@deprecate Qbounded(x) Q.bounded(x)
@deprecate Qcommutative(x) Q.commutative(x)


## We make a module Q to hold the assumptions
## this follows this page http://docs.sympy.org/0.7.5/_modules/sympy/assumptions/ask.html
module Q
import SymPy
import PyCall
if VERSION < v"0.4.0"
    eval(parse("using Docile"))
    eval(parse("Docile.@document"))
end
import Base: complex, integer, real
##http://docs.sympy.org/dev/_modules/sympy/assumptions/ask.html#ask
Q_predicates = (:antihermitian,
                :bounded, :finite, # bounded deprecated
                :commutative,
#                :complex,
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
                :nonzero,
                :positive,
                :prime,
                :real,
                :odd,
                :is_true,
                :nonpositive,
                :nonnegative,
                :symmetric,
                :invertible,
                :singular,
                :orthogonal,
                :unitary,
                :normal,
                :positive_definite,
                :upper_triangular,
                :lower_triangular,
                :diagonal,
                :triangular,
                :unit_triangular,
                :fullrank,
                :square,
                :real_elements,
                :complex_elements,
                :integer_elements)
                
for meth in Q_predicates
        nm = string(meth)
        @eval begin
            @doc """
`$($nm)`: a SymPy function.
The SymPy documentation can be found through: http://docs.sympy.org/latest/search.html?q=$($nm)
""" ->
            ($meth)(x::SymPy.SymbolicObject) = PyCall.pyeval("f(x)", f=SymPy.sympy[:Q][($nm)], x=SymPy.project(x))
        end
    end
end

export Q

