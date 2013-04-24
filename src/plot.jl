## simple plotting of expressions using GoogleCharts


as_function(ex::Sym) = u -> n(subs(ex, sym"x", u))

plot(ex::Sym, x::Sym, a::Real, b::Real) = (GoogleCharts.plot(as_function(subs(ex, x, sym"x")), a, b) | render)
plot(ex::Sym, a::Real, b::Real) = plot(ex, sym"x", a, b)

## plot( (x^2, diff(x^2, x)), 0, 4)
plot(exs::Vector{Sym}, x::Sym, a::Real, b::Real) =
    (GoogleCharts.plot(map(ex -> as_function(subs(ex, x, sym"x")), exs), a, b) | render)
plot(exs::Vector{Sym}, a::Real, b::Real) =   plot(exs, sym"x", a, b)

