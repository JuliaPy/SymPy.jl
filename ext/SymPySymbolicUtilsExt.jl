module SymPySymbolicUtilsExt

using SymPy
import SymbolicUtils

#==
Check if x represents an expression tree. If returns true, it will be assumed that operation(::T) and arguments(::T) methods are defined. Definining these three should allow use of SymbolicUtils.simplify on custom types. Optionally symtype(x) can be defined to return the expected type of the symbolic expression.
==#
function SymbolicUtils.istree(x::SymPy.SymbolicObject)
    !(x.is_Atom)
end

#==
f x is a term as defined by istree(x), exprhead(x) must return a symbol, corresponding to the head of the Expr most similar to the term x. If x represents a function call, for example, the exprhead is :call. If x represents an indexing operation, such as arr[i], then exprhead is :ref. Note that exprhead is different from operation and both functions should be defined correctly in order to let other packages provide code generation and pattern matching features.
function SymbolicUtils.exprhead(x::SymPy.SymbolicObject) # deprecated
    :call # this is not right
end
==#

#==
Returns the head (a function object) performed by an expression tree. Called only if istree(::T) is true. Part of the API required for simplify to work. Other required methods are arguments and istree
==#
function SymbolicUtils.operation(x::SymPy.SymbolicObject)
    @assert SymbolicUtils.istree(x)
    nm = Symbol(SymPy.Introspection.funcname(x))

    λ = get(SymPy.Introspection.funcname2function, nm, nothing)
    if isnothing(λ)
        return getfield(Main, nm)
    else
        return λ
    end
end


#==
Returns the arguments (a Vector) for an expression tree. Called only if istree(x) is true. Part of the API required for simplify to work. Other required methods are operation and istree
==#
function SymbolicUtils.arguments(x::SymPy.SymbolicObject)
    collect(SymPy.Introspection.args(x))
end

#==
Construct a new term with the operation f and arguments args, the term should be similar to t in type. if t is a SymbolicUtils.Term object a new Term is created with the same symtype as t. If not, the result is computed as f(args...). Defining this method for your term type will reduce any performance loss in performing f(args...) (esp. the splatting, and redundant type computation). T is the symtype of the output term. You can use SymbolicUtils.promote_symtype to infer this type. The exprhead keyword argument is useful when creating Exprs.
==#
function SymbolicUtils.similarterm(t::SymPy.SymbolicObject, f, args, symtype=nothing;
                                   metadata=nothing, exprhead=:call)
    f(args...) # default
end


end
