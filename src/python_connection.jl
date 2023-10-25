## PyCall specific usage
Base.convert(::Type{S}, x::Sym{T}) where {T<:PyCall.PyObject, S<:Sym} = x
Base.convert(::Type{S}, x::T) where {T<:PyCall.PyObject, S <: SymbolicObject} = Sym(x)

SymPyCore._convert(::Type{T}, x) where {T} = convert(T, x)
function SymPyCore._convert(::Type{Bool}, x::PyObject)
    x == _sympy_.logic.boolalg.BooleanTrue && return true
    x == _sympy_.logic.boolalg.BooleanFalse && return false
    x == PyObject(true) && return true
    x == PyObject(false) && return false
    error("Can't convert $x to boolean")
end


## Modifications for ↓, ↑
Sym(x::Nothing) = Sym(PyCall.PyObject(nothing))
Sym(x::Bool) = Sym(PyObject(x))

SymPyCore.:↓(x::PyCall.PyObject) = x
SymPyCore.:↓(d::Dict) = Dict(↓(k) => ↓(v) for (k,v) ∈ pairs(d))
SymPyCore.:↓(x::Set) = _sympy_.sets.FiniteSet((↓(xi) for xi ∈ x)...)

SymPyCore.:↑(::Type{<:AbstractString}, x) = PyObject(x)
# Create a symbolic type. There are various containers to recurse in to be
# caught here
function SymPyCore.:↑(::Type{PyCall.PyObject}, x)
    class_nm = SymPyCore.classname(x)
    class_nm == "set"   && return Set(Sym.(collect(x)))
    class_nm == "tuple" && return Tuple(↑(xᵢ) for xᵢ ∈ x)
    class_nm == "list"  && return [↑(xᵢ) for xᵢ ∈ x]
    class_nm == "dict"  && return Dict(↑(k) => ↑(x[k]) for k ∈ x)

    class_nm == "FiniteSet" && return Set(Sym.(collect(x)))
    class_nm == "MutableDenseMatrix" && return _up_matrix(x) #map(↑, x.tolist())

    # others ... more hands on than pytype_mapping

    Sym(x)
end

# ↑ for matrices
_up_matrix(m) = map(↑, m.tolist())



# hash (work around pending PyCall release with its own hash)
function Base.hash(x::SymbolicObject{T}, h::UInt) where {T <: PyCall.PyObject}
    o = ↓(x)
    px = ccall((PyCall.@pysym :PyObject_Hash), PyCall.Py_hash_t, (PyCall.PyPtr,), o) # from PyCall.jl
    reinterpret(UInt, Int(px)) - 3h                                                  # from PythonCall.jl
end


# should we also have different code path for a::String like  PyCall?
function Base.getproperty(x::SymbolicObject{T}, a::Symbol) where {T <: PyCall.PyObject}

    a == :o && return getfield(x,a)
    if a == :__pyobject__
        Base.depwarn("The field `.__pyobject__` has been renamed `.o`", :getproperty)
        return getfield(x, :o)
    end

    val = ↓(x)
    !hasproperty(val, a) && return nothing

    meth = PyCall.__getproperty(val, a)

    (meth == PyObject(nothing)) && return nothing

    if hasproperty(meth, :is_Boolean)
        o = convert(SymPyCore.Bool3, Sym(meth))
        o == true && return true
        o == false && return false
    end

    if hasproperty(meth, :__class__) && meth.__class__.__name__ == "bool"
        return convert(Bool, meth)
    end

    # treat modules, calls others differently
    if hasproperty(meth, :__class__) && meth.__class__.__name__ == "module"
        return Sym(meth)
    end

    if hasproperty(meth, :__call__)
        return SymPyCore.SymbolicCallable(meth)
    end

    return ↑(convert(PyCall.PyAny, meth))

end


# do we need this conversion?
#Base.convert(::Type{T}, o::Py) where {T <: Sym} = T(o)
