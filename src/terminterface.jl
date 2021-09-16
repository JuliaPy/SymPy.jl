using TermInterface

function TermInterface.istree(ex::Sym)
    o = ex.__pyobject__
    o.is_symbol && return false
    o.is_number && return false
    return true
end

function TermInterface.symtype(ex::Sym)
    is_(:integer) && return Integer
    is_(:real) && return Real
    is_(:complex) && return Complex
    nothing
end


TermInterface.issym(ex::Sym) = ex.__pyobject__.is_symbol
TermInterface.nameof(ex::Sym) = Symbol(ex.__pyobject__.name)
function TermInterface.exprhead(ex::Sym)
    ex.__pyobject__.__class__.__name__ == "Indexed" && return :ref
    :call
end
TermInterface.operation(ex::Sym) = SymPy.Introspection.func(ex)
TermInterface.arguments(ex::Sym) = SymPy.Introspection.args(ex)

#function TermInterface.metadata(x, data)
#function TermInterface.similarterm(ex::Sym) ... end
