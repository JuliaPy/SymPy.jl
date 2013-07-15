## methods for core



core_object_methods = (:args, :as_poly, :atoms,
                       :compare, :compare_pretty,
                       :count, :doit, :dummy_eq, 
                       :has, :match, :replace,
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
                       :count_ops, :equals, :expand, :extract_additively,
                       :extract_branch_factor, :extract_multiplicatively,
                       :getO, :getn,
                       ## :is_integer called as x[:is_intgeger]
                       :is_constant, :is_polynomial, :is_rational_function,
                       :integer_nthroot,
                       :leadterm, 
                       :primitive, 
                       :removeO, :round,
                       :series,
                       :expand
                       )


for meth in core_object_methods
    meth_name = string(meth)
    @eval ($meth)(ex::Sym, args...; kwargs...) = object_meth(ex, symbol($meth_name), args...; kwargs...)
    eval(Expr(:export, meth))
end

core_object_properties = (:assumptions0,
                          :is_even, :is_odd,
                          :is_number, :is_integer, :is_real, :is_complex, :is_rational,
                          :is_commutative, 
                          :free_symbols # a bit funny, returns a set
                          )


for prop in core_object_properties
    prop_name = string(prop)
    @eval ($prop)(ex::Sym) = ex[symbol($prop_name)]
    eval(Expr(:export, prop))
end


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


for meth in core_sympy_methods
    meth_name = string(meth)
    @eval ($meth)(ex::Sym, args...; kwargs...) = sympy_meth(symbol($meth_name), ex, args...; kwargs...)
    eval(Expr(:export, meth))
end


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