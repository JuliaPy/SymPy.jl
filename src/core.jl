## methods for core



core_object_methods = (:as_poly, :atoms,
                       :compare, :compare_pretty,
                       :doit, :dummy_eq, # :count,
                       :has, :match, ##:replace,
                       :rewrite, :sort_key,
                       :xreplace,
                       :args_cnc,
                       :as_coeff_Add, :as_coeff_Mul, :as_coeff_add,
                       :as_coeff_exponent, :as_coeff_factors, :as_coeff_mul,
                       :as_coeff_terms, :as_coefficient, :as_coefficient_dict,
                       :as_content_primitive, :as_expr, :as_independent,
                       :as_leading_term, :as_numer_denom, :as_ordered_factors,
                       :as_ordered_terms, :as_powers_dicts, :as_real_imag,
                       :as_terms,
                       :coeff, :compute_leading_term, :could_extract_minus_sign,
                       :count_ops, :equals,  :extract_additively,
                       :extract_branch_factor, :extract_multiplicatively,
                       :getO, :getn,
                       ## :is_integer called as x[:is_integer]
                       :is_constant, :is_polynomial, :is_rational_function,
                       :integer_nthroot,
                       :leadterm,
                       :primitive,
                       :removeO,
                       :series,
                       :expand
                       )


core_object_properties = (:assumptions0,
                          :is_even, :is_odd,
                          :is_number, :is_integer, :is_real, :is_complex, :is_rational,
                          :is_commutative
                          )


"""
Return a vector of free symbols in an expression
"""
free_symbols(ex::Sym) =  convert(Vector{Sym}, collect(ex.x[:free_symbols]))
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


## From relational
core_sympy_methods = (:Wild, :Dummy,
                      :Mod, :Rel,
                      :Eq, :Ne, :Lt, :Le, :Gt, :Ge,
                      :Equality, :Unequality,
                      :GreaterThan, :LessThan, :StrictGreaterThan, :StrictLessThan,
                      :PoleError,
                      :count_ops,
                      :gcdex, :half_gcdex,
                      :igcd, :ilcm
                      )

"""
Extract left and right hand side of a relation, parts of a relation.


(These are properties in SymPy, functions in SymPy.jl)

Examples:
```
x = Sym("x")
Eq(x, sin(x)) |> rhs  ## sin(x)
```
"""
rhs(ex::Sym, args...; kwargs...) = ex.x[:rhs]
lhs(ex::Sym, args...; kwargs...) = ex.x[:lhs]

"""

Returns a tuple of arguments

cf. [args](http://docs.sympy.org/latest/modules/core.html#sympy.core.basic.Basic.args)

(args is a property in SymPy, a function call in SymPy.jl.)

Examples
```
Eq(x, x^2) |> args ## (x, x^2)
sin(x) |> args ## (x,)
```
"""
args(ex::Sym) = ex.x[:args]


## need to import these
core_sympy_methods_base = (:factorial,
                           :gcd, :lcm,
                           :isqrt
                           )
for meth in core_sympy_methods_base
    meth_name = string(meth)
    @eval ($meth)(ex::Sym, args...; kwargs...) =
      sympy_meth(symbol($meth_name), ex, args...; kwargs...)
end
