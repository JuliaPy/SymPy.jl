## Plotting functions using Plots.jl

# SymPy itself has
# plot, plot3d, plot_parametric, plot3d_parametric_line, plot3d_parametric_surface, plot_implicit

# Implementations
#                                 Plots  PyPlot     Gadfly    Winston
# plot(::Sym)                        ✓     ✓           ✓          ✓
# plot(::Vector{Sym})                ✓     .           ✓          .
# plot(::Tuple{Sym})                 ✓(2D) ✓           ✓(2D)      ✓(2D)
# parametricplot                     ✓(2D) ✓           ✓(2D)      ✓(2D)
# contour(::Vector{Sym})             .     ✓           ✓          .
# contour3D(::Vector{Sym})           .     ✓           .          .
# vectorplot(::Vector{Sym})          .     ✓           .          .   ## quiver?
# plot_surface(::Vector{Sym})        .     ✓           .          .
# add_arrow(p,v)                     .     ✓           .          .
# text...                            .     .           .          .    



function _plot(fn::Function, ex::SymbolicObject, a, b; kwargs...) 
    vars = get_free_symbols(ex)
    if length(vars) == 1
        f(x) = N(subs(ex, vars[1], x))
        fn(f, a, b; kwargs...)
    elseif length(vars) == 2
        error("Expression to plot may have only one free variable")
    end
end



function _plot{T<:SymbolicObject}(fn::Function, exs::Vector{T}, a, b; kwargs...) 
    vars = get_free_symbols(maximum(exs))
    if length(vars) == 1
        fns = map(ex -> (x -> N(subs(ex, vars[1], x))), exs)
        fn(fns, a, b; kwargs...)
    elseif length(vars) == 2
        error("Expressions to plot may have only one common free variable")
    end
end

# Parametric plots
## 2d parametric plot plot((ex1, ex2), a, b)
function _plot{T<:SymbolicObject, S<:SymbolicObject}(fn::Function, exs::Tuple{T,S}, t0::Real, t1::Real, args...; kwargs...)
    vars = get_free_symbols(max(exs[1], exs[2]))
    if length(vars) == 1
        f1(x) = N(subs(exs[1], vars[1], x))
        f2(x) = N(subs(exs[2], vars[1], x))
        fn(f1, f2, t0, t1, args...; kwargs...)
    else
        error("Parametric plot may have only one free variable")
    end
end

"""

Plotting of symbolic objects.

`SymPy` uses the `Plots` package to provide a uniform interface to many of `Julia`'s plotting packages. The default one used is `Gadfly`. These methods are extended to plot symbolic objects:


* `plot(ex::Sym, a, b; kwargs...)` will plot a function evaluating `ex`

* `plot(exs::Vector{Sym}, a, b; kwargs...)` will plot the functions evaluating `exs`

* `plot((ex1, ex2), a, b; kwargs...)` will plot the two expressions in a parametric plot over the interval `[a,b]`

* `plot(ex1, ex2, a, b; kwargs...)` will plot a parametric plot of `ex1` and `ex2` over `[a,b]`

* `plot((ex1, ex2, ex3), a0, a1, b0, b1; kwargs)` will create a 3D parametric plot over `[a,b]`

* `plot(ex1, ex2, ex3, a0, a1, b0, b1; kwargs)` will create a 3D parametric plot over `[a,b]`

* `contour(ex1, x0, x1, y0, y1; kwargs...)` wiil plot a contour pot over the interval

* `plot_implicit(XXX, x0, x1, y0, y1; kwargs...)` XXX
"""
Plots.plot(ex::SymbolicObject, a, b; kwargs...) = _plot(Plots.plot, ex, a, b; kwargs...)
Plots.plot!(ex::SymbolicObject, a, b; kwargs...) = _plot(Plots.plot!, ex, a, b; kwargs...)

Plots.plot{T<:SymbolicObject}(ex::Vector{T}, a, b; kwargs...) = _plot(Plots.plot, exs, a, b; kwargs...)
Plots.plot!{T<:SymbolicObject}(ex::Vector{T}, a, b; kwargs...) = _plot(Plots.plot!, exs, a, b; kwargs...)

Plots.plot{T<:SymbolicObject, S<:SymbolicObject}(exs::Tuple{T,S}, t0::Real, t1::Real, args...; kwargs...) =
    _plot(Plots.plot, exs, t0, t1, args...; kwargs...)
Plots.plot!{T<:SymbolicObject, S<:SymbolicObject}(exs::Tuple{T,S}, t0::Real, t1::Real, args...; kwargs...) =
    _plot(Plots.plot!, exs, t0, t1, args...; kwargs...)
    
Plots.plot{T<:SymbolicObject, S<:SymbolicObject}(ex1::T, ex2::S, t0::Real, t1::Real, args...; kwargs...) =
    _plot(Plots.plot, (ex1, ex2), t0, t1, args...; kwargs...)
Plots.plot!{T<:SymbolicObject, S<:SymbolicObject}(ex1::T, ex2::S, t0::Real, t1::Real, args...; kwargs...) =
    _plot(Plots.plot!, (ex1, ex2), t0, t1, args...; kwargs...)

## surface plot
## function Plots.surface(ex::Sym, x1,x2, y1, y2, n=250,  args...; kwargs...)
##     nvars = length(get_free_symbols(ex))
##     nvars == 2 || throw(DimensionMismatch("Expression has $nvars, expecting 2 for a surface plot"))
##     xs = linspace(sort([x1,x2])..., n)
##     ys = linspace(sort([y1,y2])..., n)
##     f = convert(Function, ex)
##     zs = [convert(Float64, f(x,y)) for x in xs, y in ys]
##     Plots.surface(xs, ys, zs, args...; kwargs...)
## end


## function Plots.contour(ex::Sym, x1,x2, y1, y2, args...; kwargs...)
##     vars = get_free_symbols(ex)
##     length(vars) == 2 || error("wrong number of free variables for a contour plot")
##     f = convert(Function, ex)
    
##     xs = linspace(sort([x1,x2])...)
##     ys = linspace(sort([y1,y2])...)
##     zs = [convert(Float64, f(x,y)) for x in xs, y in ys]
##         ## Winston.contour???
##     Plots.contour(xs, ys, zs, args...; kwargs...)
## end

## function Plots.vectorplot(f::Vector{Sym}, 
##                           x1::Real=-5.0, x2::Real=5.0,
##                           y1::Real=-5.0, y2::Real=5.0,
##                           nx::Int=25, ny::Int=25, args...; kwargs...)
##     length(f) == 2 || throw(DimensionMismatch("vector of symbolic objects must have length 2"))
##     for ex in f
##         nvars = length(get_free_symbols(ex))
##         nvars == 2 || throw(DimensionMismatch("Expression has $nvars, expecting 2 for a quiver plot"))
##     end

##     f1 = (x,y) -> convert(Float64, convert(Function, f[1])(x,y))
##     f2 = (x,y) -> convert(Float64, convert(Function, f[2])(x,y))

##     xs, ys = linspace(x1, x2, nx), linspace(y1, y2, ny)
    
##     us = [f1(x,y) for x in xs, y in ys]
##     vs = [f2(x,y) for x in xs, y in ys]
    
##     Plots.vectorplot(xs, ys, us, vs, args...; kwargs...)
## end

export plot, plot! # contour, surface, contour3d, vectorplot, plot_implicit
