## Assumptions
## http://docs.sympy.org/0.7.2/modules/assumptions/index.html

"""
refine: http://docs.sympy.org/dev/modules/assumptions/refine.html
"""
refine(ex, assumpts...) = sympy_meth(:refine, ex, assumpts...)
export refine

## This is a bit messed up, as we use Qeven in place of Q.even, ...
Q_predicates = (:even, :odd, :prime, :nonzero,
                :complex, :extended_real, :imaginary, :infinitesimal, 
                :integer, :irrational, :real,
                :positive, :negative,
                :bounded, :commutative)
Q_nms = [symbol("Q" * string(i)) for i in Q_predicates]

for (fn, meth) in zip(Q_nms, Q_predicates)
    nm = string(meth)
    @eval ($fn)(x) = PyCall.pyeval("f(x)", f=sympy[:Q][($nm)], x=project(x))
    eval(Expr(:export, fn))
end


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


## ask. Returns true, false or nothing
##
## ask(Qinteger(x*y), And(Qinteger(x), Qinteger(y)))
## ## really slow isprime:
## filter(x -> ask(Qprime(x)), [1:1000])
ask(x::Sym, args...) = sympy_meth(:ask, x, args...)
export ask
