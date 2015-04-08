functions_sympy_methods = (
                           :arg,
                           :conjugate,
                           :re,
                           :sign
                           )


## map Abs->abs, Max->max, Min->min
abs(ex::Sym, args...; kwargs...) = sympy_meth(:Abs, ex, args...; kwargs...)
#minimum(ex::Sym,x::NAtype) = x
minimum(ex::Sym, args...; kwargs...) = sympy_meth(:Min, ex, args...; kwargs...)
#maximum(ex::Sym,x::NAtype) = x
maximum(ex::Sym, args...; kwargs...) = sympy_meth(:Max, ex, args...; kwargs...)

Base.real(x::Sym) = sympy.re(project(x))
Base.imag(x::Sym) = sympy.im(project(x))
