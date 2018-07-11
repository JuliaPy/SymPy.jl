## There are many Poly class methods
## Instances of Poly can be created via:
## @syms x y z
## p = poly(x^2 -x - 1)
##


const SymPoly = Sym


Base.divrem(p::Sym, q::Sym) = sympy_meth(:div, p, q)
Base.div(p::Sym, q::Sym) = divrem(p,q)[1]
Base.rem(p::Sym, q::Sym) = divrem(p,q)[2]


## rename these, their use is special to polynomials, so we prefix
## Renamed polynomial methods.

## The functions `div`, `rem, `divrem` had slightly different meaning in
## Julia than SymPy, where they are polynomial methods. Here we rename
## them by prefixing with "poly". We do the same for `roots`, so as to
## not conflict with the `roots` function from `Polynomials.jl`.

## [This isn't really needed. We do keep the `polyroots`, as `roots` does not live in base.]

"""

Polynomial division. Renamed from `div` in SymPy to avoid confusion with Julia's `div`. Also, same as `divrem`.

"""
polydiv(ex::Sym, args...; kwargs...) = sympy_meth(:div, ex, args...; kwargs...)

"""

Polynomial division remainer.

"""

polyrem(ex::Sym, args...; kwargs...) = rem(ex, args...; kwargs...)

"""

Polynomial division with remainder. Renamed from `divrem` in SymPy to avoid confusion with Julia's `divrem`

"""
polydivrem(ex::Sym, args...; kwargs...) = sympy_meth(:div, ex, args...; kwargs...)


"""

Find roots of a polynomial. Renamed from `roots` in
SymPy to avoid confusion with the `roots` function of `Polynomials`

"""
polyroots(ex::Sym, args...; kwargs...) = sympy_meth(:roots, ex, args...; kwargs...)
export polydiv, polyrem, polydivrem, polyroots



polynomial_sympy_methods = (
                            :groebner,
                            :solve_poly_system,
                            :resultant,
                            :cancel,
                            :apart, :together,
                            :poly,
                            :poly_from_expr,
                            :degree_list,
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
                            :reduced,
                            :is_zero_dimensional,
                            :symmetrize,
                            :horner,
                            :viete,
                            :construct_domain,
                            :minimal_polynomial,
                            :minpoly, :primitive_element,
                            :field_isomorphism, :to_number_field
#                            :numer#, :denom
#                            :roots ## conflict with Roots.roots and functionality provided by solve
                            )

degree(x::Sym) = sympy_meth(:degree, x)
export degree

denom(x::Sym) = sympy_meth(:denom, x)
numer(x::Sym) = sympy_meth(:numer, x)
export denom, numer
Base.denominator(x::SymbolicObject) = denom(x)
Base.numerator(x::SymbolicObject) = numer(x)

polynomial_sympy_methods_base = (:expand,
                                 :factor #,
                                    #:trunc
                                    )


## SymPy Poly class

"""

Constructor for polynomials. This allows certain polynomial-specifec
methods to be called, such as `coeffs`. There is also `poly` that does
the same thing. (We keep `Poly`, as it is named like a constructor.)

Examples:
```
p = Poly(x^2 + 2x + 1, x)  #  a poly in x over Z
coeffs(p)  # [1, 2, 1]
p = Poly(a*x^2 + b*x + c, x)  # must specify variable that polynomial is over.
coeffs(p)  ## [a,b,c]
```

"""
Poly(x::Sym) = sympy["Poly"](x)
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

"""

    coeffs(p, [x])
    
Return coefficients, `[a0, a1, ..., an]`, of a polynomial `p = a0 + a1*x + ... + an*x^n` in `D[x]`.

Note: SymPy has the `all_coeffs` function to do this for objects of class `Poly`. The SymPy `coeffs` function returns the non-zero values. Use `sympy"coeffs"` for this functionality.

This function returns the coefficients starting with the constant and
includes zeros, when present. This matches the order of the `coeffs`
function of the `Polynomials` package. This is implemented as
`reverse(all_coeffs(Poly(p,[x])))`.

The expression is converted to a object of class `Poly`. This requires a choices of symbol after which the expression is viewed as a univariate polynomial over a domain. If no symbol is passed in, a heuristic is used, for convenience: when only one free symbol is present it will be used; if a symbol "x" is present it will be used; otherwise the first symbol returned by `free_symbols` will be used. This will clearly fail for constant polynomials.

Examples:
```
@vars x a b c
ex = 2x^2 + 3x + 4
coeffs(ex)  # [4, 3, 2], same as `coeffs(ex, x)`

ex = a*x^2 + b*x + c
coeffs(ex)  # [c, b, a], same as `coeffs(ex, x)`

## may need to be explicit if `x` not present:
@vars y
ex = a*y^2 + b*y + c
coeffs(ex)  # not [c,b,a] as `c` is first free symbol
coeffs(ex, y)  # [c, b, a]
```
"""
function coeffs(p::Sym, x::Sym)
    q = Poly(p, x)
    reverse(object_meth(q, :all_coeffs))
end
function coeffs(p::Sym)
    vars = free_symbols(p)
    length(vars) == 0 && DomainError("No free variables specified")
    if length(vars) == 1
        return coeffs(p, vars[1])
    else
        for var in vars
            if "x" == string(var)
                return coeffs(p, var)
            end
        end
        return coeffs(p, vars[1])
    end
end
export coeffs


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
#                        :is_primitive,
                         :is_quadratic,
                         :is_sqf,
                         :is_univariate,
                         :is_zero)


is_primitive(x::Sym) = PyObject(x)[:is_primitive]
export is_primitive


##
"""

    interpolate([xs], ys, x)

Find interpolating polynomial in the variable x through the points (xs[1], ys[1]), ..., (xs[n], ys[n])

Examples
```jltest
@vars x
interpolate(sin.(1:4), x)          # uses xs = 1:length(ys(
interpolate(1:4, sin.(1:4), x)     # xs, ys
interpolate([(1,2), (2,3), (3,2)], x)  # [(x1,y1), ..., (xn, yn)]
```
"""
interpolate(xsys::AbstractVector, x) = sympy_meth(:interpolate, xsys, x)
interpolate(xs::AbstractVector, ys::AbstractVector, x::Sym) = interpolate(collect(zip(xs, ys)), x)
export interpolate
