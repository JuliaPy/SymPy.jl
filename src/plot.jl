## simple plotting of expressions using GoogleCharts

plot(ex::Sym, x::Sym, a::Real, b::Real) = (GoogleCharts.plot(convert(Function, subs(ex, x, sym"x")), a, b) | render)
plot(ex::Sym, a::Real, b::Real) = plot(ex, sym"x", a, b)

## plot( (x^2, diff(x^2, x)), 0, 4)
plot(exs::Tuple, x::Sym, a::Real, b::Real) =
    (GoogleCharts.plot([convert(Function, subs(ex, x, sym"x")) for ex in exs], a, b) | render)
plot(exs::Tuple, a::Real, b::Real) =   plot(exs, sym"x", a, b)

