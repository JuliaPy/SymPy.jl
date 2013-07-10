## Symbol class for controlling dispatch

abstract SymbolicObject

immutable Sym <: SymbolicObject
    x::PyCall.PyObject
end
Sym(s::SymbolicObject) = s

## Many (too many) ways to create symbolobjects
## Sym("x"), Sym(:x), Sym("x", "y") or Sym(:x, :y)

Sym(s::Union(Symbol, String)) = sympy[:symbols](string(s))
Sym(args...) = map(Sym, args)

## (a,b,c) = @syms a b c --- no commas on right hand side!
## (x,) @syms x is needed for single arguments
macro syms(x...)
    q=Expr(:block)
    for s in x
        push!(q.args, Expr(:(=), s, Expr(:call, :Sym, Expr(:quote, s))))
    end
    push!(q.args, Expr(:tuple, x...))
    q
end

macro sym_str(x)
    Sym(x)
end

## define one or more symbols directly
## a,b,c = symbols("a,b,c", commutative=false)
function symbols(x::String; kwargs...) 
    out = sympy.symbols(x; kwargs...)
end


## Automatic conversion of python types to Sym class.

basictype = sympy.basic["Basic"]
pytype_mapping(basictype, Sym)

polytype = sympy.polys["polytools"]["Poly"]
pytype_mapping(polytype, Sym)

convert(::Type{Sym}, o::PyCall.PyObject) = Sym(o)
convert(::Type{PyObject}, s::Sym) = s.x


length(x::SymbolicObject) = *(size(x)...)
function size(x::SymbolicObject)
    if pyisinstance(x.x, matrixtype)
        return x[:shape]
    else
        return ()
    end
end
function size(x::SymbolicObject, dim::Integer)
    if dim <= 0
        error("dimension out of range")
    elseif dim <= 2 && pyisinstance(x.x, matrixtype)
        return x[:shape][dim]
    else
        return 1
    end
end

## pull out x property of Sym objects or leave alone
project(x::Any) = x
project(x::SymbolicObject) = x.x
project(x::Tuple) = map(project, x)


## convert args so that we can use obj[:methname](x,...) without needed to project
## python: obj.method(arg1, arg2, ...) -> julia: obj[:meth](args...) 
## no kwargs though!
function getindex(x::SymbolicObject, i::Symbol)
    out = project(x)[i]
    isa(out, Function) ? (args...) -> out(project(args)...) : out
end


## format
## Can only have pretty for non-array objects, o/w priting is all messed up

## as values use embedded \n values to align which don't have any idea of a cell
show(io::IO, s::SymbolicObject) = print(io, sympy.pretty(project(s)))

## need to call pretty on SymPy matrix object, not julia matrix of sympy objects
show(io::IO, s::Array{Sym}) =  print(io, summary(s), "\n", convert(Sym, s))
repl_show(io::IO, s::Vector{Sym}) =  print(io, summary(s), "\n", convert(Sym, s))





doc(x::SymbolicObject) = print(x[:__doc__]())

_str(s::SymbolicObject) = s[:__str__]()
_str(a::Array{SymbolicObject}) = map(_str, a)

pprint(s::SymbolicObject, args...) = sympy[:pprint](project(s), project(args)...)
latex(s::SymbolicObject, args...)  = sympy[:latex ](project(s), project(args)...)

function jprint(x::SymbolicObject)
  out = PyCall.pyeval("str(x)", x = x.x)

  if ismatch(r"\*\*", out)
    return replace(out, "**", "^")
  else
    return out
  end
end

## Convert SymPy symbol to Julia expression
convert(::Type{Expr}, x::SymbolicObject) = parse(jprint(x))

## Number types
promote_rule{T <: Number}(::Type{Sym}, ::Type{T}) = Sym
convert{T <: Real}(::Type{T}, x::Sym) = convert(T, project(x))
convert(::Type{String},  x::Sym) = convert(String,  project(x))
convert(::Type{Rational}, s::Sym) = Rational(project(s)[:p], project(s)[:q])
convert(::Type{Complex}, x::Sym) = complex(map(float, x[:as_real_imag]())...)


complex(x::Sym) = convert(Complex, x)
complex(xs::Array{Sym}) = map(complex, xs)


## Conversion to function a bit tricky as we *assume* variable is called x.
## can use subs(xsym, sym"u", sym"x") first if it is u, say.
convert(::Type{Function}, xsym::Sym) = u -> float(subs(xsym, sym"x", u))


## Various means to call sympy or object methods. All convert input,
## not all convert output.
##
## we may have sympy.method
## or we may have object.method

## Makes it possible to call in a sympy method, witout worrying about Sym objects
call_sympy_fun(fn::Function, args...; kwargs...) = fn(map(project, args)...; [(k,project(v)) for (k,v) in kwargs]...)
sympy_meth(meth::Symbol, args...; kwargs...) = call_sympy_fun(sympy[meth], args...; kwargs...)

## meth of object, convert arguments
object_meth(object::SymbolicObject, meth::Symbol, args...; kwargs...) =  
  call_sympy_fun(project(object)[meth],  args...; kwargs...)


## meth of object, convert arguments, output to SymMatrix 
function call_matrix_meth(object::SymbolicObject, meth::Symbol, args...; kwargs...) 
    out = object_meth(object, meth, args...; kwargs...)
    if isa(out, SymMatrix) 
        convert(Array{Sym}, out)
    elseif  length(out) == 1
        out 
    else
        map(u -> isa(u, SymMatrix) ? convert(Array{Sym}, u) : u, out)
    end
end



## From PyCall.pywrap:
function members(o::Union(PyObject, Sym))
    out = convert(Vector{(String,PyObject)}, 
                  pycall(PyCall.inspect["getmembers"], PyObject, project(o)))
    String[u[1] for u in out]
end
