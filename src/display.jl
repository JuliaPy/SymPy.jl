## Display code


"""
Access SymPy's docstrings

There are several calling styles, as finding the underlying SymPy object from
a Julia object is a bit tricky.

Examples
```
@vars x
import Base.Docs.doc
doc(sin(x))        #
doc(sympy[:sin])   # explicit module lookup
doc(SymPy.mpmath[:hypercomb]) # explicit module lookup
doc(Poly(x^2,x), :coeffs) # coeffs is an object method of the poly instance
doc([x 1;1 x], :LUsolve)  # LUsolve is a matrix method
```
"""
Base.Docs.doc(x::SymbolicObject) = Base.Docs.doc(PyObject(x))
Base.Docs.doc(x::SymbolicObject, s::Symbol) = Base.Docs.doc(PyObject(x)[s])
Base.Docs.doc(x::Array{T,N}, s::Symbol) where {T <: SymbolicObject, N} = Base.Docs.doc(PyObject(x)[s])

## Add some of SymPy's displays
## Some pretty printing


#doc(x::SymbolicObject) = print(x[:__doc__])

"Map a symbolic object to a string"
_str(s::SymbolicObject) = s[:__str__]()

"Map an array of symbolic objects to a string"
_str(a::AbstractArray{SymbolicObject}) = map(_str, a)

"call SymPy's pretty print"
pprint(s::SymbolicObject, args...; kwargs...) = sympy_meth(:pprint, s, args...; kwargs...)

"Call SymPy's `latex` function. Not exported. "
latex(s::SymbolicObject, args...; kwargs...)  = sympy_meth(:latex, s, args...; kwargs...)

"create basic printed output"
function jprint(x::SymbolicObject)
    out = PyCall.pycall(pybuiltin("str"), String, PyObject(x))
    out = replace(out, r"\*\*" => "^")
    out
end
jprint(x::AbstractArray) = map(jprint, x)

## show is called in printing tuples, ...
## we would like to use pprint here, but it does a poor job on complicated multi-line expressions
Base.show(io::IO, s::Sym) = print(io, jprint(s))

## We add show methods for the REPL (text/plain) and IJulia (text/latex)

## text/plain
show(io::IO, ::MIME"text/plain", s::SymbolicObject) =  print(io, sympy["pretty"](s))
show(io::IO, ::MIME"text/latex", x::Sym) = print(io, latex(x, mode="equation*"))

function  show(io::IO, ::MIME"text/latex", x::AbstractArray{Sym})
    function toeqnarray(x::Vector{Sym})
        a = join([latex(x[i]) for i in 1:length(x)], "\\\\")
        """\\[ \\left[ \\begin{array}{r}$a\\end{array} \\right] \\]"""
#        "\\begin{matrix}$a\\end{matrix}"
    end
    function toeqnarray(x::AbstractArray{Sym,2})
        sz = size(x)
        a = join([join(map(latex, x[i,:]), "&") for i in 1:sz[1]], "\\\\")
        "\\[\\left[ \\begin{array}{" * repeat("r",sz[2]) * "}" * a * "\\end{array}\\right]\\]"
#        "\\begin{bmatrix}$a\\end{bmatrix}"
    end
    print(io, toeqnarray(x))
end

## Pretty print dicts
function show(io::IO, ::MIME"text/latex", d::Dict{T,S}) where {T<:Sym, S<:Any}
    Latex(x::Sym) = latex(x)
    Latex(x) = sprint(io -> show(IOContext(io, :compact => true), x))

    out = "\\begin{equation*}\\begin{cases}"
    for (k,v) in d
        out = out * Latex(k) * " & \\text{=>} &" * Latex(v) * "\\\\"
    end
    out = out * "\\end{cases}\\end{equation*}"
    print(io, out)
end



## Convert SymPy symbol to Julia expression
convert(::Type{Expr}, x::SymbolicObject) = Meta.parse(jprint(x))
