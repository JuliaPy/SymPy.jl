## port methods of series module of SymPy
## http://docs.sympy.org/0.7.2/modules/series.html

## simple methods (x, args) -> y (y coercion happens via PyCall)
series_sympy_meths = (:limit,
                      :gruntz,
                      :series, :Order, :O,
                      :residue
                      )



for meth in series_sympy_meths
    meth_name = string(meth)
    @eval ($meth)(ex::Sym, args...; kwargs...) = call_meth(symbol($meth_name), ex, args...; kwargs...)
    eval(Expr(:export, meth))
end

