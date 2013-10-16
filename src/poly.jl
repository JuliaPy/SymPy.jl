## There are many Poly class methods
## Instances of Poly can be created via:
## x, y, z = @syms x y z
## p = poly(x^2 -x - 1)
##


const SymPoly = Sym


polynomial_sympy_methods = (:div, :rem,
                            :sqf_list,
                            :groebner,
                            :solve_poly_system,
                            :resultant,
                            :cancel,
                            :apart, :together,
                            :poly,
                            :poly_from_expr,
                            :degree, :degree_list,
                            :LC, :LM, :LT,
                            :pdiv, :prem, :pquo, :pexquo, 
                            :quo, :exquo,
                            :half_gcdex, :gcdex,
                            :invert,
                            :subresultants,
                            :discriminant,
                            :terms_gcd,
                            :cofactors,
                            :gcd_list,
                            :lcm_list,
                            :monic,
                            :content,
                            :compose, :decompose,
                            :sturm,
                            :gff_list, :gff,
                            :sqf_norm, :sqf_part, :sqf_list, :sqf,
                            :factor_list,
                            :intervals,
                            :refine_root,
                            :count_roots,
                            :real_roots,
                            :nroots,
                            :ground_roots,
                            :nth_power_roots_poly,
                            :reduced,
                            :is_zero_dimensional,
                            :symmetrize,
                            :horner,
                            :interpolate,
                            :viete,
                            :construct_domain,
                            :minimal_polynomial,
                            :minpoly, :primitive_element,
                            :field_isomorphism, :to_number_field,
                            :roots
                            )
                            



polynomial_sympy_methods_import = (:expand,
                                   :factor #,
                                   #:trunc
                                   )
                            
for meth in polynomial_sympy_methods_import
    meth_name = string(meth)
    @eval ($meth)(ex::Sym, args...; kwargs...) = sympy_meth(symbol($meth_name), ex, args...; kwargs...)
end

## Poly class methods that aren't sympy methods
## e.g., must be called as obj.method(...), but not method(obj, ...)       
## not coeffs(x^2 -1), rather poly(x^2 - 1)  |> coeffs
polynomial_instance_methods = (:EC, :ET, :LC, :LM, :LT, :TC, ## no .abs()
                              :add_ground, 
                               :all_coeffs,
                              :all_monoms, :all_roots, :all_terms, 
                              :as_dict, :as_expr, :as_list,
                              :clear_denoms,
                              :coeffs,
                              :deflate,
                              :exclude,
                              :l1_norm,
                              :lift,
                              :ltrim,
                              :max_norm,
                              :monoms,
                              :mul_ground,
                              :neg,
                              :nth, :nth_power_roots_poly,
                              :per,
                              :rat_clear_denoms,
                              :retract,
                              :root,
                              :set_domain, :set_modulus,
                              :sqr, :sub_ground,
                              :to_exact, :to_field, :to_ring, :total_degree,
                              :unify
                              )



polynomial_predicates = (
                         :is_cyclotomic,
                         :is_ground,
                         :is_homogeneous,
                         :is_irreducible,
                         :is_linear,
                         :is_monic,
                         :is_monomial,
                         :is_multivariate,
                         :is_one,
                         :is_primitive,
                         :is_quadratic,
                         :is_sqf,
                         :is_univariate,
                         :is_zero)





## special cases due to output

## {Sym => val} dictionary
#roots(p::Sym, args...; kwargs...) = sympy.roots(project(p), args...; kwargs...)