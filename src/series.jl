## port methods of series module of SymPy
## http://docs.sympy.org/0.7.2/modules/series.html

## simple methods (x, args) -> y (y coercion happens via PyCall)
series_sympy_meths = (#:limit
                      :gruntz,
                      :series, :Order, :O,
                      :residue,
                      :fourier_series
                      )

import Base: truncate

if VERSION < v"0.6.0-dev"
    eval(Expr(:import, :Base, :scale))
else
     eval(Expr(:export, :scale))
end
series_object_meths_base = (:truncate,
                            )

series_object_meths = (:shift, :shiftx, :scalex)



