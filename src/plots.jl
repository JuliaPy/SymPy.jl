


## Plotting functions using Plots.jl
import Plots.plotter!
plotter!(:pyplot) # default, not :gadfly

## XXX Need to add plot of [ex1, ex2, ...] -> [f1, f2, ..., fn]

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


```
A summary:
                                Plots  PyPlot     Gadfly    Winston
plot(::Sym)                        ✓     ✓           ✓          ✓
plot(::Vector{Sym})                ✓     .           ✓          .
plot(::Tuple{Sym})                 ✓(2D) ✓           ✓(2D)      ✓(2D)
```

* `contour(ex1, x0, x1, y0, y1; kwargs...)` wiil plot a contour plot over the interval

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




## """

## There are other graphics not currently supported with the `Plots` package that can be accessed by loading an underlying plotting package.

## ```
## Implementations
##                                 Plots  PyPlot     Gadfly    Winston
## plot(::Sym)                        ✓     ✓           ✓          ✓
## plot(::Vector{Sym})                ✓     .           ✓          .
## plot(::Tuple{Sym})                 ✓(2D) ✓           ✓(2D)      ✓(2D)
## contour(::Vector{Sym})             .     ✓           ✓          .
## contour3D(::Vector{Sym})           .     ✓           .          .
## vectorplot(::Vector{Sym})          .     ✓           .          .   
## plot_surface(::Vector{Sym})        .     ✓           .          .
## add_arrow(p,v)                     .     ✓           .          ✓
## ```
## """


## Others must be done within packages, as *currently* there is no support for these graphics in Plots.jl
## hopefully this will change over time and this code can be merged in there.
## This has issues:
## * The @require macro will only work if we actually load `PyPlot`. This causes a warning with `plot` and
##   needs SymPy to be loaded before PyPlot.
function init_plot()
    Requires.@require PyPlot begin
        info("Using PyPlot graphics with SymPy")

        ## parametric line plot in 3d
        function Plots.plot{T<:SymbolicObject, S<:SymbolicObject, R<:SymbolicObject}(exs::Tuple{T,S,R}, t0::Real, t1::Real, args...; kwargs...)
            ## XXX how to make a new plot here?
            out = prepare_parametric(exs, t0, t1)
            PyPlot.plot3D(out..., args...; kwargs...)
        end
        Plots.plot(ex1::SymbolicObject, ex2::SymbolicObject, ex3::SymbolicObject, t0::Real, t1::Real, args...; kwargs...) = plot((ex1, ex2, ex3), t0, t1, args...; kwargs...)
        
        function Plots.plot!{T<:SymbolicObject, S<:SymbolicObject, R<:SymbolicObject}(exs::Tuple{T,S}, t0::Real, t1::Real, args...; kwargs...)
            ## XXX how to add to current plot here?            
            out = prepare_parametric(exs, t0, t1)
            PyPlot.plot3D(out..., args...; kwargs...)
        end
        Plots.plot!(ex1::SymbolicObject, ex2::SymbolicObject, ex3::SymbolicObject, t0::Real, t1::Real, args...; kwargs...) = plot!((ex1, ex2, ex3), t0, t1, args...; kwargs...)
        

        PyPlot.contour(ex::Sym, x1,x2, y1, y2, args...; kwargs...) = begin
            xs = linspace(sort([x1,x2])...)
            ys = linspace(sort([y1,y2])...)
            f = convert(Function, ex)
            zs = [convert(Float64, f(x,y)) for x in xs, y in ys]
            PyPlot.contour(xs, ys, zs, args...; kwargs...)
        end
        
        function PyPlot.contour3D(ex::Sym, x1,x2, y1, y2, args...; kwargs...)
            xs = linspace(sort([x1,x2])...)
            ys = linspace(sort([y1,y2])...)
            f = convert(Function, ex)
            zs = [convert(Float64, f(x,y)) for x in xs, y in ys]
            PyPlot.contour3D(xs, ys, zs, args...; kwargs...)
        end

        ## quiver ,,,http://matplotlib.org/examples/pylab_examples/quiver_demo.html
        global  vectorplot(f::Vector{Sym}, 
                           x1::Real=-5.0, x2::Real=5.0,
                           y1::Real=-5.0, y2::Real=5.0,
                           nx::Int=25, ny::Int=25, args...; kwargs...) = begin
                               
                               length(f) == 2 || throw(DimensionMismatch("vector of symbolic objects must have length 2"))
                               for ex in f
                                   nvars = length(get_free_symbols(ex))
                                   nvars == 2 || throw(DimensionMismatch("Expression has $nvars, expecting 2 for a quiver plot"))
                               end

                               f1 = (x,y) -> convert(Float64, convert(Function, f[1])(x,y))
                               f2 = (x,y) -> convert(Float64, convert(Function, f[2])(x,y))

                               xs, ys = linspace(x1, x2, nx), linspace(y1, y2, ny)

                               us = [f1(x,y) for x in xs, y in ys]
                               vs = [f2(x,y) for x in xs, y in ys]

                               quiver(xs, ys, us, vs, args...; kwargs...)
                           end

        function PyPlot.plot_surface(ex::Sym, x1::Real,x2::Real, y1::Real, y2::Real, n=250,  args...; kwargs...)
            nvars = length(get_free_symbols(ex))
            nvars == 2 || throw(DimensionMismatch("Expression has $nvars, expecting 2 for a surface plot"))
            xs = linspace(sort([x1,x2])..., n)
            ys = linspace(sort([y1,y2])..., n)
            f = convert(Function, ex)
            zs = [convert(Float64, f(x,y)) for x in xs, y in ys]
            PyPlot.plot_surface(xs, ys, zs, args...; kwargs...)
        end

        global add_arrow(p::Vector, v::Vector, args...; kwargs...) = begin
            n = length(p)
            if n == 2
                PyPlot.arrow(p..., v...; kwargs...)
            elseif n==3
                out = [hcat(p,p+v)'[:,i] for i in 1:n]
                PyPlot.plot3D(out..., args...; kwargs...)
            end
        end

        eval(Expr(:export, :contour))
        eval(Expr(:export, :contour3d))
        eval(Expr(:export, :contour3d))
        eval(Expr(:export, :vectorplot))
        eval(Expr(:export, :plot_surface))
        eval(Expr(:export, :add_arrow))

    end

    Requires.@require Gadfly begin
        info("Using Gadfly")

        global contour(ex::Sym, x1::Real,x2::Real, y1::Real, y2::Real, args...; kwargs...) = begin
            f = convert(Function, ex)
            Gadfly.plot((x,y) -> convert(Float64, f(x,y)), x1, x2, y1, y2, args...; kwargs...)
        end


        eval(Expr(:export, :contour))
    end
    
    Requires.@require Winston begin
        info("Using Winston")

        global add_arrow(p::Vector, v::Vector) = begin
            n = length(p)
            p,v = map(x -> convert(Float64, x), p), map(x -> convert(Float64, x), v)
            n == 2 || error("Winston is only 2 dimensional")
            oplot([p[1], p[1] + v[1]], [p[2], p[2] + v[2]])
        end
        eval(Expr(:export, :add_arrow))        
    end
    
end
## function Plots.surface(ex::Sym, x1,x2, y1, y2, n=250,  args...; kwargs...)
##     nvars = length(get_free_symbols(ex))
##     nvars == 2 || throw(DimensionMismatch("Expression has $nvars, expecting 2 for a surface plot"))
##     xs = linspace(sort([x1,x2])..., n)
##     ys = linspace(sort([y1,y2])..., n)
##     f = convert(Function, ex)
##     zs = [convert(Float64, f(x,y)) for x in xs, y in ys]
##     Plots.surface(xs, ys, zs, args...; kwargs...)
## end


## function contour(ex::Sym, x1,x2, y1, y2, args...; kwargs...)
##     vars = get_free_symbols(ex)
##     length(vars) == 2 || error("wrong number of free variables for a contour plot")
##     f = convert(Function, ex)
 
##     xs = linspace(sort([x1,x2])...)
##     ys = linspace(sort([y1,y2])...)
##     zs = [convert(Float64, f(x,y)) for x in xs, y in ys]
##         ## Winston.contour???
##     contour(xs, ys, zs, args...; kwargs...)
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

export plot, plot!, contour # contour, surface, contour3d, vectorplot, plot_implicit
