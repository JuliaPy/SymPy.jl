## simple plotting of expressions of a single free variable using Gadfly
## by removing this, the package will not depend on Gadfly and DataFrames -- this should load much faster

#using DataFrames
#using Gadfly



function plot(ex::Sym, a::Real, b::Real) 
    tmp = convert(Function, ex)
    Gadfly.plot(x -> float(tmp(x)), a, b)
end

## plot( [x^2, diff(x^2, x)], 0, 4)
function plot(exs::Vector{Sym}, a::Real, b::Real) 
    tmp = map(ex -> convert(Function, ex), exs)
    tmp = map(f -> (x -> float(f(x))), tmp)
    Gadfly.plot(tmp, a, b)
end

