

## Many (too many) ways to create symbolobjects
## Sym("x"), Sym(:x), Sym("x", "y") or Sym(:x, :y), @syms x y, symbols("x y")

Sym(s::Union(Symbol, String)) = sympy.symbols(string(s))
Sym{T <: Number}(s::T) = convert(Sym, sympy.sympify(s))
Sym(args...) = map(Sym, args)

## (a,b,c) = @syms a b c --- no commas on right hand side!
## (x,) @syms x is needed for single arguments
## Thanks to vtjnash for this!
macro syms(x...)
    q=Expr(:block)
    if length(x) == 1 && isa(x[1],Expr)
        @assert x[1].head === :tuple "@syms expected a list of symbols"
        x = x[1].args
    end 
    for s in x
        @assert isa(s,Symbol) "@syms expected a list of symbols"
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


length(x::SymbolicObject) = *(size(x)...)
function size(x::SymbolicObject)
    return ()
end
function size(x::SymbolicObject, dim::Integer)
    if dim <= 0
        error("dimension out of range")
   
    else
        return 1
    end
end

## pull out x property of Sym objects or leave alone
project(x::Any) = x
project(x::SymbolicObject) = x.x
project(x::Symbol) = project(Sym(x)) # can use :x instead of Sym(x)
project(x::Tuple) = map(project, x)


## convert args so that we can use obj[:methname](x,...) without needed to project
## python: obj.method(arg1, arg2, ...) -> julia: obj[:meth](args...) 
## no kwargs though!
##
## Examples:
## ```
## x = Sym("x")
## (x^2 - 2x + 1)[:diff]()
## (x^2 - 2x + 1)[:integrate]((x,0,1))
## ```
function getindex(x::SymbolicObject, i::Symbol)
    ## find method
    if haskey(project(x), i)
        out = project(x)[i]
        if isa(out, Function) 
            function f(args...;kwargs...) 
                out(project(args)...;kwargs...)
            end
            return f
        else
            return out
        end
    elseif i in names(sympy)
        out = sympy.(i)
        if isa(out, Function) 
            function f(args...;kwargs...) 
                out(project(x), project(args)...;kwargs...) 
            end
            return f
        else
            return out
        end
    else
        MethodError()
    end
end


## format
## Can only have pretty for non-array objects, o/w priting is all messed up

## as values use embedded \n values to align which don't have any idea of a cell
show(io::IO, s::SymbolicObject) = print(io, sympy.pretty(project(s)))

## need to call pretty on SymPy matrix object, not julia matrix of sympy objects
show(io::IO, s::Array{Sym}) =  print(io, summary(s), "\n", convert(Sym, s))






doc(x::SymbolicObject) = print(x[:__doc__])

_str(s::SymbolicObject) = s[:__str__]()
_str(a::Array{SymbolicObject}) = map(_str, a)

pprint(s::SymbolicObject, args...; kwargs...) = sympy.pprint(project(s), project(args)...;  [(k,project(v)) for (k,v) in kwargs]...)
latex(s::SymbolicObject, args...; kwargs...)  = sympy.latex(project(s), project(args)...;  [(k,project(v)) for (k,v) in kwargs]...)

function jprint(x::SymbolicObject)
  out = PyCall.pyeval("str(x)", x = x.x)

  if ismatch(r"\*\*", out)
    return replace(out, "**", "^")
  else
    return out
  end
end
jprint(x::Array) = map(jprint, x)

## Convert SymPy symbol to Julia expression
convert(::Type{Expr}, x::SymbolicObject) = parse(jprint(x))

## Number types
promote_rule{T <: Number}(::Type{SymbolicObject}, ::Type{T}) = Sym
promote_rule{T <: Number}(::Type{Sym}, ::Type{T}) = Sym
convert{T <: Real}(::Type{T}, x::Sym) = convert(T, project(x))

convert(::Type{Sym}, x::Number)  = sympy.Symbol(string(x))
convert(::Type{SymbolicObject}, x::Number) = sympy.Symbol(string(x))
convert(::Type{Sym}, x::Rational) = sympy.Rational(x.num, x.den)
convert(::Type{SymbolicObject}, x::Rational) = sympy.Rational(x.num, x.den)
convert(::Type{Sym}, x::Complex) = real(x) == 0 ? sympy.Symbol("$(imag(x))*I") : sympy.Symbol("$(real(x)) + $(imag(x))*I")
convert(::Type{String},  x::Sym) = convert(String,  project(x))
convert(::Type{Rational}, s::Sym) = Rational(project(s)[:p], project(s)[:q])
convert(::Type{Complex}, x::Sym) = complex(map(float, x[:as_real_imag]())...)


complex(x::Sym) = convert(Complex, x)
complex(xs::Array{Sym}) = map(complex, xs)


## Conversion to function a bit hacky
## we use free_symbols to get the free symbols, then create a function
## with arguments in this order. No problem with only one variable, but
## may be confusing when more than one in ex.
function convert(::Type{Function}, ex::Sym)
    free = free_symbols(ex)
    vars = [free[:pop]()]
    for i in 1:free[:__len__]()
        push!(vars, free[:pop]())
    end
    len = length(vars)
    local out
    (args...) -> begin
        out = ex
        for i in 1:length(vars)
            out = out[:subs](vars[i], args[i])
        end
        out
    end
end

## Various means to call sympy or object methods. All convert input,
## not all convert output.
##
## we may have sympy.method
## or we may have object.method

## Makes it possible to call in a sympy method, witout worrying about Sym objects
call_sympy_fun(fn::Function, args...; kwargs...) = fn(map(project, args)...; [(k,project(v)) for (k,v) in kwargs]...)
function sympy_meth(meth::Symbol, args...; kwargs...) 
    ans = call_sympy_fun(sympy.(meth), args...; kwargs...)
    ## make nicer...
    if isa(ans, Vector)
        ans = Sym[i for i in ans]
    end
    ans
end
        

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


## add writemime support
## how to pass "mode" to writemime
import Base.writemime
export writemime
## various ways to write out mime equations
writemime(io::IO, ::MIME"text/latex", x::Sym) = print(io, latex(x, mode="equation*", itex=true))
function writemime(io::IO, ::MIME"text/latex", x::Array{Sym}) 
    function toeqnarray(x::Vector{Sym})
        a = join([latex(x[i]) for i in 1:length(x)], "\\\\")
        "\\begin{bmatrix}$a\\end{bmatrix}"
    end
    function toeqnarray(x::Array{Sym,2})
        sz = size(x)
        a = join([join(map(latex, x[i,:]), "&") for i in 1:sz[1]], "\\\\")
        "\\begin{bmatrix}$a\\end{bmatrix}"
    end
    print(io, toeqnarray(x))
end
 
## attempt to write out a dict. Likely not too robust, but simple cases look good.
## Not sure this belongs here ...
function writemime(io::IO, ::MIME"text/latex", d::Dict)    
    Latex(x::Sym) = latex(x)
    Latex(x) = string(x)

    out = "\\begin{equation*}\\begin{cases}"
    for (k,v) in d
        out = out * Latex(k) * " & \\text{=>} &" * Latex(v) * "\\\\"
    end
    out = out * "\\end{cases}\\end{equation*}"
    print(io, out)


end
