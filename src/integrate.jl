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


integrals_instance_methods = (:as_sum,
                              :limits,
                              :transform,
                              :variables)
                              
  

                           

## Alternate interface for simple integral
integrate(s::Sym, x::Sym, from::Union(Real, Sym), to::Union(Real, Sym)) = integrate(s, (x, from, to))

## conveniences for integrating a univariate function, if possible
function integrate(f::Function)
    x = Sym("x")
    integrate(f(x), x)
end

function integrate(f::Function, from::Union(Real, Sym), to::Union(Real, Sym))
    x = Sym("x")
    integrate(f(x), x, from, to)
end



## summation
summations_sympy_methods = (
                            :summation, :Sum
                            )

summations_instance_methods  = (
                                :euler_maclaurin,
                                )
summations_object_properties  = (:is_zero, :is_number)

