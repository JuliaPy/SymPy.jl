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

# return Julia function (not pycall)
struct VarargWrapper{F}
    fn::F
end
(F::VarargWrapper)(xs...) = F.fn(xs)

op_map = Base.Dict{String,Any}(
"Add" => +,
              "Sub" => -,
              "Mul" => *, # (SymPy.__PROD__)
              "Div" => /,
              "Pow" => ^,
              "re"  => real,
              "im"  => imag,
              "Abs" => abs,
              "Min" => min,
              "Max" => max,
              "Poly" => identity,
              "Piecewise" => _piecewise,
              "Order" => (as...) -> 0,
    "And" => VarargWrapper(all), #(&),
              "Or" =>  VarargWrapper(any), #(|),
              "Less" => (<),
              "LessThan" => (<=),
              "StrictLessThan" => (<),
              "Equal" => (==),
              "Equality" => (==),
              "Unequality" => (!==),
              "StrictGreaterThan" => (>),
              "GreaterThan" => (>=),
              "Greater" => (>),
    "conjugate" => conj,
    "atan2" => atan,
    "Heaviside" =>  __HEAVISIDE__,

)

function _expr_to_generic(ex)
    ux = convert(Expr, ex) #sympy.julia_code(ex)
    #eval(operation(Meta.parse(ux)))
    eval(operation(ux))
end

function map_op(key, ex, op_map)
    haskey(op_map, key) ? op_map[key] : _expr_to_generic(ex)
end

# return function, not symbol
function TermInterface.operation(ex::Sym)
    fname = SymPy.Introspection.funcname(ex)
    map_op(fname, ex, op_map)
end



TermInterface.arguments(ex::Sym) = SymPy.Introspection.args(ex)

#function TermInterface.metadata(x, data)
TermInterface.istree(::Type{Sym})=true
