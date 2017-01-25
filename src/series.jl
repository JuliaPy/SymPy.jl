## port methods of series module of SymPy
## http://docs.sympy.org/0.7.2/modules/series.html

## simple methods (x, args) -> y (y coercion happens via PyCall)
series_sympy_meths = (#:limit
                      :gruntz,
                      :series, :Order, :O,
                      :residue,
                      :fourier_series
                      )

import Base: scale, truncate
series_object_meths_base = (:truncate,
                            :scale)

series_object_meths = (:shift, :shiftx, :scalex)



