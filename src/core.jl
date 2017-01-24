## methods for core
core_object_methods = (:as_poly, :atoms,
                       :compare, :compare_pretty,
                       :doit, :dummy_eq, # :count,
                       :has, 
                       :sort_key,
                       :args_cnc,
                       :as_coeff_Add, :as_coeff_Mul, :as_coeff_add,
                       :as_coeff_exponent, :as_coeff_factors, :as_coeff_mul,
                       :as_coeff_terms, :as_coefficient, :as_coefficient_dict,
                       :as_content_primitive, :as_expr, :as_independent,
                       :as_leading_term, :as_numer_denom, :as_ordered_factors,
                       :as_ordered_terms, :as_powers_dicts, :as_real_imag,
                       :as_terms,
                       :coeff, :compute_leading_term, :could_extract_minus_sign,
                       :equals,  :extract_additively,
                       :extract_branch_factor, :extract_multiplicatively,
                       :getO, :getn,
                       ## :is_integer called as x[:is_integer]
                       :is_constant, :is_polynomial, :is_rational_function,
                       :integer_nthroot,
                       :leadterm,
                       :primitive,
                       :removeO
                       )


core_object_properties = (:assumptions0,
                          :is_even, :is_odd,
                          :is_number, :is_integer, :is_real, :is_complex, :is_rational,
                          :is_commutative
                          )


## From relational
core_sympy_methods = (:sympify, :flatten,
                      :Dummy,
                      :Mod, :Rel,
                      :Eq, :Ne, :Lt, :Le, :Gt, :Ge,
                      :Equality, :Unequality,
                      :PoleError,
                      :count_ops,
                      :gcdex, :half_gcdex,
                      :igcd, :ilcm
                      )

## these are defined for Reals, not Sym, Real... Necessary to lambdify Indicators as of v0.6.0-dev
for meth in (:GreaterThan, :LessThan, :StrictGreaterThan, :StrictLessThan)
    meth_name = string(meth)
    @eval begin
        @doc """
`$($meth_name)`: a SymPy function.
    The SymPy documentation can be found through: http://docs.sympy.org/latest/search.html?q=$($meth_name)
""" ->
        ($meth)(a::Real, b::Real) = sympy_meth($meth_name,a, b)
    end
    eval(Expr(:export, meth))
end


"""
Extract left and right hand side of a relation, parts of a relation.


(These are properties in SymPy, functions in SymPy.jl)

Examples:
```
x = Sym("x")
Eq(x, sin(x)) |> rhs  ## sin(x)
```
"""
rhs(ex::Sym, args...; kwargs...) = PyObject(ex)[:rhs]
lhs(ex::Sym, args...; kwargs...) = PyObject(ex)[:lhs]




"""
Return a vector of free symbols in an expression
"""
function free_symbols(ex::Sym)
    fs = PyObject(ex)[:free_symbols]
    ## are these a set?
    if fs[:__class__][:__name__] == "set"
        convert(Vector{Sym}, collect(fs))
    else
        Sym[]
    end
end
function free_symbols{T<:SymbolicObject}(exs::Vector{T})
    as = map(free_symbols, exs)
    out = as[1]
    if length(as) > 1
        for j in 2:length(as)
            for u in as[j]
                u in out || push!(out, u)
            end
        end
    end
    out
end
function free_symbols(exs::Tuple)
    as = map(free_symbols, exs)
    out = as[1]
    if length(as) > 1
        for j in 2:length(as)
            for u in as[j]
                u in out || push!(out, u)
            end
        end
    end
    out
end
export free_symbols

