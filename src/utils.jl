immutable Sym
    x::PyCall.PyObject
end
Sym(s::Sym) = s

## Sym("x"), Sym(:x), Sym("x", "y") or Sym(:x, :y)
## have to add Sym here, as conversion isn't working below
Sym(s::Union(Symbol, String)) = sympy[:symbols](string(s))
#Sym(s::Union(Symbol, String)) = Sym(sympy[:symbols](string(s)))
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
function symbols(x::String; commutative::Bool=true) 
    out = sympy.symbols(x, commutative=commutative)
    length(out) > 1 ? map(u -> convert(Sym, u), out) : out
end

basictype = sympy.basic["Basic"]
pytype_mapping(basictype, Sym)

matrixtype = sympy.matrices["MatrixBase"]
pytype_mapping(matrixtype, Sym)

polytype = sympy.polys["polytools"]["Poly"]
pytype_mapping(polytype, Sym)

convert(::Type{Sym}, o::PyCall.PyObject) = Sym(o)
convert(::Type{PyObject}, s::Sym) = s.x




length(x::Sym) = *(size(x)...)
function size(x::Sym)
    if pyisinstance(x.x, matrixtype)
        return x[:shape]
    else
        return ()
    end
end
function size(x::Sym, dim::Integer)
    if dim <= 0
        error("dimension out of range")
    elseif dim <= 2 && pyisinstance(x.x, matrixtype)
        return x[:shape][dim]
    else
        return 1
    end
end

project(x::Any) = x
project(x::Sym) = x.x
project(x::Tuple) = map(project, x)
project(x::Array{Sym}) = project(convert(SymMatrix, x))

## convert args so that we can use obj[:methname](x,...) without needed to project
function getindex(x::Sym, i::Symbol)
    out = project(x)[i]
    isa(out, Function) ? (args...) -> out(project(args)...) : out
end


## format
## Can only have pretty for non-array objects, o/w priting is all messed up

## as values use embedded \n values to align which don't have any idea of a cell
show(io::IO, s::Sym) = print(io, sympy.pretty(project(s)))

## need to call pretty on SymPy matrix object, not julia matrix of sympy objects
show(io::IO, s::Array{Sym}) =  print(io, summary(s), "\n", convert(Sym, s))
repl_show(io::IO, s::Vector{Sym}) =  print(io, summary(s), "\n", convert(Sym, s))



_str(s::Sym) = s[:__str__]()
_str(a::Array{Sym}) = map(_str, a)

pprint(s::Sym, args...) = sympy[:pprint](project(s), project(args)...)
latex(s::Sym, args...)  = sympy[:latex ](project(s), project(args)...)

function jprint(x::Sym)
  out = PyCall.pyeval("str(x)", x = x.x)

  if ismatch(r"\*\*", out)
    return replace(out, "**", "^")
  else
    return out
  end
end

promote_rule{T <: Number}(::Type{Sym}, ::Type{T}) = Sym
convert{T <: Real}(::Type{T}, x::Sym) = convert(T, project(x))
convert(::Type{String},  x::Sym) = convert(String,  project(x))
function convert(::Type{Rational}, x::Sym)
    ## issues with conversion: compare convert(Rational, sympy.harmonic(30)) to sympy.harmonic(30)
    out = fraction(x)
    int(out[1]) // int(out[2])
end
    




convert(::Type{Complex}, x::Sym) = complex(map(float, x[:as_real_imag]())...)


complex(x::Sym) = convert(Complex, x)
complex(xs::Array{Sym}) = map(complex, xs)


## Conversion to function a bit tricky as we *assume* variable is called x.
## can use subs(xsym, sym"u", sym"x") first if it is u, say.
convert(::Type{Function}, xsym::Sym) = u -> float(subs(xsym, sym"x", u))

## Convert SymPy symbol to Julia expression
convert(::Type{Expr}, x::Sym) = parse(jprint(x))

## Various means to call sympy or object methods. All convert input, not all convert output.

## Makes it possible to call in a sympy method, witout worrying about Sym objects
call_sympy_fun(fn::Function, args...; kwargs...) = fn(map(project, args)...; [(k,project(v)) for (k,v) in kwargs]...)
## hyperexpand(args...; kwargs...) = call_meth(:hyperexpand, args...; kwargs...)
## convert arguments
sympy_meth(meth::Symbol, args...; kwargs...) = call_sympy_fun(sympy[meth], args...; kwargs...)
## convert arguments, output
call_meth(meth::Symbol, args...; kwargs...) = convert(Sym, sympy_meth(meth, args...; kwargs...))
## meth of object, convert arguments
object_meth(object::Sym, meth::Symbol, args...; kwargs...) =  call_sympy_fun(project(object)[meth],  args...; kwargs...)
## meth of object, convert arguments, output
function call_object_meth(object::Sym, meth::Symbol, args...; kwargs...)
    out = object_meth(object, meth, args...; kwargs...)
    convert(Sym, out)
end
## meth of object, convert arguments, output to SymMatrix 
function call_matrix_meth(object::Sym, meth::Symbol, args...; kwargs...) 
    out = object_meth(object, meth, args...; kwargs...)
    out = convert(SymMatrix, out)
    convert(Array{Sym}, out)
end



## From PyCall.pywrap:
function members(o::Union(PyObject, Sym))
    out = convert(Vector{(String,PyObject)}, 
                  pycall(PyCall.inspect["getmembers"], PyObject, project(o)))
    String[u[1] for u in out]
end



doc(x::Sym) = print(x[:__doc__]())
