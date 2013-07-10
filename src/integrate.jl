## http://docs.sympy.org/0.7.2/modules/integrals/integrals.html


integrals_sympy_methods = (:integrate,
                           :mellin_transform,
                           :inverse_mellin_transform,
                           :lapace_transform,
                           :inverse_lapace_transform,
                           :fourier_transform,
                           :inverse_fourier_transform,
                           :sine_transform,
                           :inverse_sine_transform,
                           :cosine_transform,
                           :inverse_cosine_transform,
                           :hankel_transform,
                           :inverse_hankel_transform,
                           :line_integrate,
                           :deltaintegrate,
                           :ratint,
                           :heurisch,
                           :trigintegrate
                           )

for meth in integrals_sympy_methods
    meth_name = string(meth)
    @eval ($meth)(ex::Sym, args...; kwargs...) = sympy_meth(symbol($meth_name), ex, args...; kwargs...)
    eval(Expr(:export, meth))
end


integrals_instance_methods = (:as_sum,
                              :free_symbols,
                              :limits,
                              :transform,
                              :variables)
                              
  
for meth in integrals_instance_methods
    meth_name = string(meth)
    @eval ($meth)(ex::Sym, args...; kwargs...) = object_meth(ex, symbol($meth_name), args...; kwargs...)
    eval(Expr(:export, meth))
end
                         


                           

## Alternate interface for simple integral
integrate(s::Sym, x::Sym, from::Real, to::Real) = integrate(s, (x, from, to))
