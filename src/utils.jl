type Sym
    x::PyCall.PyObject
    function Sym(x::Union(Symbol, String))
        new(sympy[:symbols](string(x)))
    end
    Sym(x::PyCall.PyObject) = new(x)
end

Sym(args...) = map(Sym, args)

macro sym_str(x)
    Sym(x)
end

length(x::Sym) = 1

project(x::Any) = x
project(x::Sym) = x.x
project(x::Tuple) = map(project, x)

## convert args so that we can use obj[:methname](x,...) without needed to project
function getindex(x::Sym, i::Symbol)
    out = project(x)[i]
    isa(out, Function) ? (args...) -> out(project(args)...) : out
end


## format
show(io::IO, s::Sym) = print(io, sympy.pretty(project(s))) #pprint(s)
_str(s::Sym) = s[:__str__]()
_str(a::Array{Sym}) = map(_str, a)

pprint(s::Sym, args...) = sympy[:pprint](project(s), project(args)...)
latex(s::Sym, args...)  = sympy[:latex ](project(s), project(args)...)

convert{T <: Real}(::Type{T}, x::Sym) = convert(T, project(x))
convert(::Type{String},  x::Sym) = convert(String,  project(x))

convert(::Type{Complex}, x::Sym) = complex(map(float, x[:as_real_imag]())...)
complex(x::Sym) = convert(Complex, x)
complex(xs::Array{Sym}) = map(complex, xs)


## Conversion to function a bit tricky as we *assume* variable is called x.
## can use subs(xsym, sym"u", sym"x") first if it is u, say.
convert(::Type{Function}, xsym::Sym) = u -> float(subs(xsym, sym"x", u))


## call method of a symoblic instance
call_meth(x::Sym, meth::Symbol, args::Tuple) = Sym(project(x)[meth](project(args)))
call_meth(x::Sym, meth::Symbol, args...) = Sym(project(x)[meth](project(args)...))

## From PyCall.pywrap:
function members(o::Union(PyObject, Sym))
    out = convert(Vector{(String,PyObject)}, 
                  pycall(PyCall.inspect["getmembers"], PyObject, project(o)))
    String[u[1] for u in out]
end



doc(x::Sym) = print(x[:__doc__]())
