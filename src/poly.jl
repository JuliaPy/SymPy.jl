## There are many Poly class methods
## Instances of Poly can be created via:
## @syms x y z
## p = poly(x^2 -x - 1)
##


const SymPoly = Sym

## rename these, their use is special to polynomials, so we prefix
## Renamed polynomial methods.

## The functions `div`, `rem, `divrem` had slightly different meaning in
## Julia than SymPy, where they are polynomial methods. Here we rename
## them by prefixing with "poly". We do the same for `roots`, so as to
## not conflict with the `roots` function from `Polynomials.jl`.

"""

Polynomial division. Renamed from `div` in SymPy to avoid confusion with Julia's `div`

"""
polydiv(ex::Sym, args...; kwargs...) = sympy_meth(:div, ex, args...; kwargs...)

"""

Polynomial division remainerd. Renamed from `rem` in SymPy to avoid confusion with Julia's `rem`

"""
polyrem(ex::Sym, args...; kwargs...) = sympy_meth(:rem, ex, args...; kwargs...)

"""

Polynomial division with remainder. Renamed from `divrem` in SymPy to avoid confusion with Julia's `divrem`

"""
polydivrem(ex::Sym, args...; kwargs...) = sympy_meth(:divrem, ex, args...; kwargs...)

"""

Find roots of a polynomial. Renamed from `roots` in
SymPy to avoid confusion with the `roots` function of `Polynomials`

"""
polyroots(ex::Sym, args...; kwargs...) = sympy_meth(:roots, ex, args...; kwargs...)
export polydiv, polyrem, polydivrem, polyroots



polynomial_sympy_methods = (
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
                            :numer, :denom
#                            :roots ## conflict with Roots.roots and functionality provided by solve
                            )



polynomial_sympy_methods_import = (:expand,
                                   :factor #,
                                   #:trunc
                                   )

for meth in polynomial_sympy_methods_import
    meth_name = string(meth)
    @eval ($meth)(ex::Sym, args...; kwargs...) = sympy_meth(symbol($meth_name), ex, args...; kwargs...)
end

## SymPy Poly class

"""

Constructor for polynomials. This allows certain polynomial-specifec
methods to be called, such as `coeffs`. There is also `poly` that does
the same thing. (We keep `Poly`, as it is named like a constructor.)

Examples:
```
p = Poly(x^2 + 2x + 1)  #  a poly in x over Z
coeffs(p)  # [1, 2, 1]
p = Poly(a*x^2 + b*x + c, x)  # must specify variable that polynomial is over.
coeffs(p)  ## [a,b,c]
```

"""
Poly(x::Sym) = sympy[:Poly](x)
Poly(x::Sym, args...; kwargs...) = sympy_meth(:Poly, x, args...; kwargs...)
export Poly

## Poly class methods that aren't sympy methods
## e.g., must be called as obj.method(...), but not method(obj, ...)
## not coeffs(x^2 -1), rather poly(x^2 - 1)  |> coeffs
## or poly(a*x^2 + b*x + c, x) |> coeffs to specify a specific variable
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
