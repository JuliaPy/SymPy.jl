## Assumptions
## http://docs.sympy.org/0.7.2/modules/assumptions/index.html

## This is a bit messed up, as we use Qeven in place of Q.even, ...

Q_predicates = (:even, :odd, :prime, :nonzero,
                :complex, :extended_real, :imaginary, :infinitesimal, 
                :integer, :irrational, :real,
                :positive, :negative,
                :bounded, :commutative)
Q_nms = [symbol("Q" * string(i)) for i in Q_predicates]

for (fn, meth) in zip(Q_nms, Q_predicates)
    nm = string(meth)
    @eval ($fn)(x) = PyCall.pyeval("f(x)", f=sympy.Q[($nm)], x=project(x))
    eval(Expr(:export, fn))
end


## should we support & and | for (sym,sym) pairs? Not sure
## dependso on what happens to | (x==0) and ex == x usage for ==
## for now, we can combine terms logically with And, Or, Not...
## these are in logic module


## simple methods (x, args) -> y (y coercion happens via PyCall)
logic_sympy_meths = (
                     :And, :Or, :Not, 
                     :Xor, :Nand, :Nor, :Implies,
                     :Equivalent,
                     :satisfiable
                     )

for meth in logic_sympy_meths
    meth_name = string(meth)
    @eval ($meth)(ex::Sym, args...; kwargs...) = call_meth(symbol($meth_name), ex, args...; kwargs...)
    eval(Expr(:export, meth))
end

## ask. Returns true, false or nothing
##
## ask(Qinteger(x*y), And(Qinteger(x), Qinteger(y)))
## ## really slow isprime:
## filter(x -> ask(Qprime(x)), [1:1000])
ask(x::Sym, args...) = sympy.ask(x.x, map(project, args)...)
export ask