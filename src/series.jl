## port methods of series module of SymPy
## http://docs.sympy.org/0.7.2/modules/series.html

## simple methods (x, args) -> y (y coercion happens via PyCall)
series_sympy_meths = (:limit,
                      :gruntz,
                      :series, :Order, :O,
                      :residue
                      )

## fourier series
import Base: scale, truncate
export fourier_series
export shift, shiftx, scalex

fourier_series(ex::Sym, args...; kwargs...) = sympy_meth(:fourier_series, ex, args...; kwargs...)
truncate(ex::Sym; n=3) = object_meth(ex, :truncate, n=n)
shift(ex::Sym, s) = object_meth(ex, :shift, s)
shiftx(ex::Sym, x) = object_meth(ex, :shiftx, x)
scale(ex::Sym, s::AbstractArray) = scale(ex, s[1])
scale(ex::Sym, s) = object_meth(ex, :scale, s)
scalex(ex::Sym, x) = object_meth(ex, :scalex, x)




