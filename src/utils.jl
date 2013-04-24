type Sym
    x::PyCall.PyObject
    function Sym(x::Union(Symbol, String))
        new(sympy[:symbols](string(x)))
    end
    Sym(x::PyCall.PyObject) = new(x)
end

macro sym_str(x)
    Sym(x)
end

Sym(args...) = map(Sym, args)

project(x::Any) = x
project(x::Sym) = x.x
project(x::Tuple) = map(project, x)

## convert args so that we can use obj[:methname](x,...) without needed to project
function getindex(x::Sym, i::Symbol)
    out = project(x)[i]
    isa(out, Function) ? (args...) -> out(project(args)...) : out
end


## format
show(io::IO, s::Sym) = pprint(s)
pprint(s::Sym, args...) = sympy[:pprint](project(s), project(args)...)
latex(s::Sym, args...)  = sympy[:latex ](project(s), project(args)...)

convert(String,  x::Sym) = convert(String,  project(x))
convert(Complex, x::Sym) = convert(Complex, project(x))
convert(Real,    x::Sym) = convert(Real,    project(x))
convert(Rational,x::Sym) = convert(Rational,project(x))
convert(Integer, x::Sym) = convert(Integer, project(x))


## Conversion to function a bit tricky as we *assume* variable is called x.
## can use subs(xsym, sym"u", sym"x") first if it is u, say.
convert(::Type{Function}, xsym::Sym) = u -> n(subs(xsym, sym"x", u))
convert(Sym, x::Sym) = x

## call method of a symoblic instance
call_meth(x::Sym, meth::Symbol, args::Tuple) = Sym(project(x)[meth](project(args)))
call_meth(x::Sym, meth::Symbol, args...) = Sym(project(x)[meth](project(args)...))

## From PyCall.pywrap:
function members(o::Sym)
    out = convert(Vector{(String,PyObject)}, 
                  pycall(PyCall.inspect["getmembers"], PyObject, project(o)))
    String[u[1] for u in out]
end
