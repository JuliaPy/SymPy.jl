## Alternate constructors for symbolic objects
##
## Many (too many) ways to create symbolobjects
## Sym("x"), Sym(:x), Sym("x", "y") or Sym(:x, :y), @syms x y, @vars x y, symbols("x y")

"Create a symbolic object from a symbol or string"
Sym(s::AbstractString) = sympy_meth(:sympify, s)
Sym(s::Symbol) = Sym(string(s))


"Create a symbolic number"
Sym(x::T)  where  {T <: Number} = convert(Sym, x)

## math constants in math.jl and done in __init__ stage

"vectorized version of `Sym`"
Sym(args...) = map(Sym, args)



## define one or more symbols directly
## a,b,c = symbols("a,b,c", commutative=false)
"""

Function to create one or more symbolic objects. These are specified with a string,
with commas separating different variables.

This function allows the passing of assumptions about the variables
such as `positive=true`, `real=true` or `commutative=true`. See [SymPy
Docs](http://docs.sympy.org/dev/modules/core.html#module-sympy.core.assumptions)
for a complete list.

Example:

```
x,y,z = symbols("x, y, z", real=true)
```

"""
function symbols(x::AbstractString; kwargs...)
    out = sympy_meth(:symbols, x; kwargs...)
end
symbols(x::Symbol; kwargs...) = symbols(string(x); kwargs...)


## @syms a b c --- no commas on right hand side!
## Thanks to vtjnash for this!
"""
    @syms x [y z ...] [assumptions...]
    
Macro to create many symbolic objects at once.

The `@syms` macros creates the variables and assigns them into the
local scope.

Example:

```
@syms a b c
```

Additionally you can pass assumptions on using keyword arguments:

```
@syms a positive=true b real=true c
```

Additionally you can rename arguments using pairs notation:

```
@syms   Ld=>"L_d" Lq=>"L_q"
```

The `@vars` macro is similar, `@syms` is the name used in
MATLAB.
    
Original macro magic contributed by @vtjnash and extended by @alhirzel and
@spencerlyon2
"""
macro syms(x...)
    q = Expr(:block)
    as = []    # running list of assumptions to be applied
    ss = []    # running list of symbols created
    for s in reverse(x)
        if isa(s, Expr)    # either an assumption or a named variable
            if s.head == :(=)
                s.head = :kw
                push!(as, s)
            elseif s.head == :(=>)
                push!(ss, s.args[1])
                push!(q.args, Expr(:(=), esc(s.args[1]), Expr(:call, :symbols, s.args[2], map(esc,as)...)))
            end
        elseif isa(s, Symbol)   # raw symbol to be created
            push!(ss, s)
            push!(q.args, Expr(:(=), esc(s), Expr(:call, :symbols, Expr(:quote, s), map(esc,as)...)))
        else
            throw(AssertionError("@syms expected a list of symbols and assumptions"))
        end
    end
    push!(q.args, Expr(:tuple, map(esc,reverse(ss))...)) # return all of the symbols we created
    q
end


# TODO alias @vars to @syms
"""
    @vars x y z
  
The `vars` macro is identical to  `@syms`.

"""
macro vars(x...)
    q = Expr(:block)
    as = []    # running list of assumptions to be applied
    ss = []    # running list of symbols created
    for s in reverse(x)
        if isa(s, Expr)    # either an assumption or a named variable
            if s.head == :(=)
                s.head = :kw
                push!(as, s)
            elseif s.head == :(=>)
                push!(ss, s.args[1])
                push!(q.args, Expr(:(=), esc(s.args[1]), Expr(:call, :symbols, s.args[2], map(esc,as)...)))
            end
        elseif isa(s, Symbol)   # raw symbol to be created
            push!(ss, s)
            push!(q.args, Expr(:(=), esc(s), Expr(:call, :symbols, Expr(:quote, s), map(esc,as)...)))
        else
            throw(AssertionError("@syms expected a list of symbols and assumptions"))
        end
    end
    push!(q.args, Expr(:tuple, map(esc,reverse(ss))...)) # return all of the symbols we created
    q
end

## length of object
function length(x::SymbolicObject)
    haskey(PyObject(x), :length) && return PyObject(x)[:length]
    sz = size(x)
    length(sz) == 0 && return(0)
    *(sz...)
end

## size
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


## Iterator for Sym
import Base.iterate
iterate(x::Sym) = (x.x, 0)
iterate(x::Sym, state) = nothing






"""

In SymPy, the typical calling pattern is `obj.method` or
`sympy.method` ... In `PyCall`, this becomes `obj[:method](...)` or
`sympy.method(...)`. In `SymPy` many -- but no where near all --
method calls become `method(obj, ...)`. For those that aren't
included, this allows the call to follow `PyCall`, and be
`obj[:method]` where a symbol is passed for the method name.

These just dispatches to `sympy_meth` or `object_meth`, as
appropriate. This no longer can be used to access properties of the
underlying `PyObject`. For that, there is no special syntax beyond
`object.x[:property]`.

Examples:
```
x = Sym("x")
(x^2 - 2x + 1)[:diff]()
(x^2 - 2x + 1)[:integrate]((x,0,1))
```

"""
function getindex(x::SymbolicObject, i::Symbol)
    if haskey(PyObject(x), i)
        function __XXxxXX__(args...;kwargs...) # replace with generated name
            object_meth(x, i, args...; kwargs...)
        end
        return __XXxxXX__
    elseif haskey(sympy, i)
        function __XXyyXX__(args...;kwargs...)
            sympy_meth(i, x, args...; kwargs...)
        end
        return __XXyyXX__
    else
       MethodError()
    end
end 

## Override this so that using symbols as keys in a dict works
Base.hash(x::Sym) = hash(PyObject(x))


## Helper function from PyCall.pywrap:
function members(o::Union{PyObject, Sym})
    out = pycall(PyCall.inspect["getmembers"], PyObject, o)
    AbstractString[a for (a,b) in out]
end

" Return class name as a string "
classname(ex::Sym) = PyObject(ex)[:__class__][:__name__]


