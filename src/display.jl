## Display code

## Add some of SymPy's displays
## Some pretty printing
doc(x::SymbolicObject) = print(x[:__doc__])

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
    out = PyCall.pycall(pybuiltin("str"), Compat.UTF8String, PyObject(x))
    if ismatch(r"\*\*", out)
        out = replace(out, "**", "^")
    end
    out
end
jprint(x::AbstractArray) = map(jprint, x)

## show is called in printing tuples, ...
## we would like to use pprint here, but it does a poor job on complicated multi-line expressions
Base.show(io::IO, s::Sym) = print(io, jprint(s))
Base.show(io::IO, s::AbstractArray{Sym}) = print(io, "\n", sympy_meth(:pretty, s))

## We add show methods for the REPL (text/plain) and IJulia (text/latex)

## text/plain
@compat show(io::IO, ::MIME"text/plain", s::SymbolicObject) =  print(io, sympy["pretty"](s))
@compat show(io::IO, ::MIME"text/plain", s::AbstractArray{Sym}) =  print(io, summary(s), "\n", sympy_meth(:pretty, s))

@compat show(io::IO, ::MIME"text/latex", x::Sym) = print(io, latex(x, mode="equation*", itex=true))
@compat function  show(io::IO, ::MIME"text/latex", x::AbstractArray{Sym})
    function toeqnarray(x::Vector{Sym})
        a = join([latex(x[i]) for i in 1:length(x)], "\\\\")
        "\\begin{bmatrix}$a\\end{bmatrix}"
    end
    function toeqnarray(x::AbstractArray{Sym,2})
        sz = size(x)
        a = join([join(map(latex, x[i,:]), "&") for i in 1:sz[1]], "\\\\")
        "\\begin{bmatrix}$a\\end{bmatrix}"
    end
    print(io, toeqnarray(x))
end

## Pretty print dicts
@compat function show{T<:Any, S<:Any}(io::IO, ::MIME"text/latex", d::Dict{T,S})
    Latex(x::Sym) = latex(x)
    Latex(x) = sprint(Base.showcompact, x)

    out = "\\begin{equation*}\\begin{cases}"
    for (k,v) in d
        out = out * Latex(k) * " & \\text{=>} &" * Latex(v) * "\\\\"
    end
    out = out * "\\end{cases}\\end{equation*}"
    print(io, out)
end



## Convert SymPy symbol to Julia expression
convert(::Type{Expr}, x::SymbolicObject) = parse(jprint(x))
