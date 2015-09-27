## add plotting commands for various packages (Winston, PyPlot, Gadfly)
## This is a bit of a mess
## Going forward it would be nice to work with the `Plots.jl` abstraction for the backends
## However, this will require adding some more plot types to that package.
## So for now, if `Plots` is loaded, plotting will work for some graphs as it should for plots
## of functions, only symbolic expressions are used. For other functionality, the graphing
## package should be required.

using Requires ## for @require macro

export plot_implicit, plot_parametric, plot3d,
       plot3d_parametric_line, plot3d_parametric_surface
       ## also
       ## parametricplot, vectorplot, add_arror (PyPlot)
       ## parametricplot, contour (Gadfly)
       ## paramtericplot (Winston)

# plot(ex::Sym, a, b, args; kwargs...)  function plot
# plot([ex1, ex2, ...], a,b, args...; kwargs...) layer functions
# parametricplot([x,y,z], a, b, args...;kwargs)
# plot((ex1,ex2,[ex3]), a, b, args; kwargs...) aka parametric plot
# vectorplot(f, x1, x2, y1, y2, nx, ny, args...; kwargs...)  (quiver is optional name?)
# contour(ex::Sym, x1, x2, y1, y2, args; kwargs...) contour plot
# plot_surface(ex::Sym, x1, x2, y1, y2, args...; kwargs...) (plot3D is mathematica, but not matplotlib)
# add_arrow(p::Vector, v::Vector) ## not symbolic!

## Implementations
#                                PyPlot     Gadfly    Winston
# plot(::Sym)                     ✓           ✓          ✓
# plot(::Vector{Sym})             .           ✓          .
# plot(::Tuple{Sym})              ✓           ✓(2D)      ✓(2D)
# parametricplot                  ✓           ✓(2D)      ✓(2D)
# contour(::Vector{Sym})          ✓           ✓          .
# contour3D(::Vector{Sym})        ✓           .          .
# vectorplot(::Vector{Sym})       ✓           .          .
# plot_surface(::Vector{Sym})     ✓           .          .
# add_arrow(p,v)                  ✓           .          .
# text...                         .           .          .

function prepare_parametric(exs, t0, t1)
    n = length(exs)
    (n==2) | (n==3) || throw(DimensionMismatch("parametric plot requires the initial tuple to have 2 or 3 variables"))
    vars = [free_symbols(ex) for ex in exs]
    m,M = extrema(map(length,vars))
    (m == M) & (m == 1) || throw(DimensionMismatch("parametric plot requires exactly one free variable"))
    for i in 1:(n-1)
        vars[i] == vars[i+1] || error("parametric plot's free variable must be the same")
    end

    ts = linspace(t0, t1, 250)
    [Float64[convert(Float64, convert(Function, exs[i])(t)) for t in ts] for i in 1:n] # [[xs...], [ys...], [zs...]]
end



    ## SymPy.plotting also implements things

"""

The SymPy Python module implements many plotting interfaces and
displays them with matplotlib or textually.  We refer to
http://docs.sympy.org/latest/modules/plotting.html for the
details. Here we export the main functionality that is not otherwise
given.

"""

"""

Make an implicit plot of an expression.

To create conditions on the variable, the functions `Lt`, `Le`, `Eq`,
`Ge`, and `Gt` can be used. For infix notation, unicode operators can
be used: `\ll<tab>`, `\le<tab>`, `\Equal<tab>`, `\ge<tab>`, and
`\gg<tab>`.

To combine terms, the unicode `\vee<tab>` (for "or"), `\wedge<tab>` (for "and") can be used

Warning, if using in `IJulia`, must be used **after** loading `PyPlot`.


Examples:

```
x,y = symbols("x,y", real=true)
plot_implicit(sin(x+y) - cos(x^2 + y^2), (x, -5,5), (y, -5, 5))
plot_implicit(x ≫ y)  # over default region of [-5,5] x [-5,5]
f(x,y) = x^2 + y^2
plot_implicit((f(x,y) ≪ 5) ∧ (f(x,y) ≥ 2), (x,-5,5), (y,-5,5))
```
"""
plot_implicit(ex, args...; kwargs...) = sympy[:plotting][:plot_implicit](ex.x, project(args)...;  [(k,project(v)) for (k,v) in kwargs]...)
    

"""

Make a parametric plot of the two expressions.

Warning, if using in `IJulia`, must be used **after** loading `PyPlot`.

Example:

```
t = symbols("t", real=true)
plot_parametric(sin(t), cos(t), (t, 0, 2pi))
```
"""
plot_parametric(ex1, ex2, args...; kwargs...) = sympy[:plotting][:plot_parametric](ex1.x, ex2.x, project(args)...;  [(k,project(v)) for (k,v) in kwargs]...)


"""

Make 3d plot from an expression.

Warning, if using in `IJulia`, must be used **after** loading `PyPlot`.

Note, this is not `plot3D`, which is from `PyPlot.jl`.

Examples

```
x,y = symbols("x,y", real=true)
plot3d(x^2 - 2x*y, (x, -5,5), (y, -5,5))
```
"""    
plot3d(ex, args...; kwargs...) = sympy[:plotting][:plot3d](ex.x, project(args)...;  [(k,project(v)) for (k,v) in kwargs]...)


"""

parametric lines in 3 dimensions in SymPy.

```
t = sym"t"
plot(cos(t), sin(t), t, (t, 0, 4PI))
```

"""    
plot3d_parametric_line(ex1, ex2, ex3, args...; kwargs...) = sympy[:plotting][:plot3d_parametric_line](ex1.x, ex2.x, ex3.x, project(args)...;  [(k,project(v)) for (k,v) in kwargs]...)

"""

Parametric surfaces in SymPy

Warning, if using in `IJulia`, must be used **after** loading `PyPlot`.

```
u,v = symbols("u, v", real=true)
plot3d_parametric_surface(cos(u + v), sin(u - v), u-v, (u, -5, 5), (v, -5,5))
```
"""
plot3d_parametric_surface(ex1, ex2, ex3, args...; kwargs...) = sympy[:plotting][:plot3d_parametric_surface](ex1.x, ex2.x, ex3.x, project(args)...;  [(k,project(v)) for (k,v) in kwargs]...)



    
## Must put Requires.require outside of compilation
function init_plot()

## Try to support Winston, PyPlot, and Gadfly to varying degrees
## Basically our goal here is to massage the data and let args... and kwargs.. be from the
## plotting packages.

Requires.@require Winston begin
    function Winston.plot(ex::Sym, a, b, args...; kwargs...)
        vars = free_symbols(ex)
        if length(vars) == 1
            f = convert(Function, ex)
            plot(x -> convert(Float64, f(x)), a, b, args...; kwargs...)
        elseif length(vars) == 2
            f = convert(Function, ex)
            error("Make contour plot")
        end
    end
    function Winston.oplot(ex::Sym, args...; kwargs...)
        vars = free_symbols(ex)
        if length(vars) == 1
            f = convert(Function, ex)
            oplot(x -> convert(Float64, f(x)), args...; kwargs...)
        end
    end

    ## Parametric plot
    function Winston.plot(exs::(Sym...), t0::Real, t1::Real, args...; kwargs...)
        out = prepare_parametric(exs, t0, t1)
        n = length(exs)
        if n == 2
            plot(out..., args...; kwargs...)
        elseif n == 3
            error("No 3D plotting in Winston")
        end
    end

    function contour(ex::Sym, x1,x2, y1, y2, args...; kwargs...)
        vars = free_symbols(ex)
        length(vars) == 2 || error("wrong number of free variables for a contour plot")
        f = convert(Function, ex)

        xs = linspace(sort([x1,x2])...)
        ys = linspace(sort([y1,y2])...)
        zs = [convert(Float64, f(x,y)) for x in xs, y in ys]
        ## Winston.contour???
        contour(xs, ys, zs, args...; kwargs...)
    end
    
    global parametricplot(f::Vector{Sym}, a::Real, b::Real, args...; kwargs...) = plot(tuple(f...), a, b, args...;kwargs...)

    global add_arrow(p::Vector, v::Vector) = begin
        n = length(p)
        p,v = map(x -> convert(Float64, x), p), map(x -> convert(Float64, x), v)
        n == 2 || error("Winston is only 2 dimensional")
        oplot([p[1], p[1] + v[1]], [p[2], p[2] + v[2]])
    end
    
    eval(Expr(:export, :parametricplot))
    eval(Expr(:export, :contour))
end


  Requires.@require PyPlot begin
    function PyPlot.plot(ex::Sym, a::Real, b::Real, n=250, args...; kwargs...)
        vars = free_symbols(ex)
        if length(vars) <= 1
            f = convert(Function, ex)
            xs = linspace(a,b, n)
            ys = map(x->convert(Float64,f(x)), xs)
            plot(xs, ys, args...; kwargs...)
        elseif length(vars) == 2
            contour(ex, a, b, args...; kwargs...)
        end
    end

    function PyPlot.plot(exs::Array{Sym,1},a::Real,b::Real,n=250, args...; kwargs...)
        xs = linspace(a,b,n)
        fs = map(ex->convert(Function, ex), exs)
        for i in 1:length(fs)
            plot(xs, map(x->N(fs[i](x)),xs))
        end
    end

    ## Parametric plots use notation plot((ex1,ex2, [ex3]), t0, t1, args..., kwargs...)
    function PyPlot.plot(exs::(Sym...), t0::Real, t1::Real, args...; kwargs...)
        out = prepare_parametric(exs, t0, t1)
        n = length(exs)
        if n == 2
            plot(out..., args...; kwargs...)
        elseif n == 3
            plot3D(out..., args...; kwargs...)
        end
    end

    global parametricplot(f::Vector{Sym}, a::Real, b::Real, args...; kwargs...) = plot(tuple(f...), a, b, args...;kwargs...)
                                                          
    ## quiver ,,,http://matplotlib.org/examples/pylab_examples/quiver_demo.html
    global  vectorplot(f::Vector{Sym}, 
                    x1::Real=-5.0, x2::Real=5.0,
                    y1::Real=-5.0, y2::Real=5.0,
                    nx::Int=25, ny::Int=25, args...; kwargs...) = begin

        length(f) == 2 || throw(DimensionMismatch("vector of symbolic objects must have length 2"))
        for ex in f
            nvars = length(free_symbols(ex))
            nvars == 2 || throw(DimensionMismatch("Expression has $nvars, expecting 2 for a quiver plot"))
        end

        f1 = (x,y) -> convert(Float64, convert(Function, f[1])(x,y))
        f2 = (x,y) -> convert(Float64, convert(Function, f[2])(x,y))

        xs, ys = linspace(x1, x2, nx), linspace(y1, y2, ny)

        us = [f1(x,y) for x in xs, y in ys]
        vs = [f2(x,y) for x in xs, y in ys]

        quiver(xs, ys, us, vs, args...; kwargs...)
    end
        
    function PyPlot.contour(ex::Sym, x1,x2, y1, y2, args...; kwargs...)
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

    function plot_surface(ex::Sym, x1,x2, y1, y2, n=250,  args...; kwargs...)
        nvars = length(free_symbols(ex))
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
         arrow(p..., v...; kwargs...)
       elseif n==3
         out = [hcat(p,p+v)'[:,i] for i in 1:n]
         plot3D(out..., args...; kwargs...)
       end
     end

        eval(Expr(:export, :parametricplot))
        eval(Expr(:export, :vectorplot))
        eval(Expr(:export, :add_arrow))
    
end


Requires.@require Gadfly begin
        ## import Gadfly: plot

    function Gadfly.plot(ex::Sym, a::Real, b::Real, args...; kwargs...)
        vars = free_symbols(ex)
        if length(vars) <= 1
            f = convert(Function, ex)
            plot(x -> convert(Float64, f(x)), a, b, args...; kwargs...)
        elseif length(vars) == 2
            contour(ex, a, b, args...; kwargs...)
        end
    end

    function Gadfly.plot(exs::Array{Sym,1},a::Real,b::Real,args::Union(Gadfly.Element,DataType,Gadfly.Theme,Function)...; kwargs...)
        fs = map(ex->convert(Function, ex), exs)
        plot(fs, a, b, args...; kwargs...)
    end

    ## Parametric plots use notation plot((ex1,ex2, [ex3]), t0, t1, args..., kwargs...)
    function Gadfly.plot(exs::(Sym...), t0::Real, t1::Real, args...; kwargs...)
        out = prepare_parametric(exs, t0, t1)
        n = length(exs)
        if n == 2
            plot(x=out[1],y=out[2], args...; kwargs...)
        elseif n == 3
            error("No 3D plotting in Gadfly")
        end
    end

    global parametricplot(f::Vector{Sym}, a::Real, b::Real, args...; kwargs...) = plot(tuple(f...), a, b, args...;kwargs...)
    
    function contour(ex::Sym, x1::Real,x2::Real, y1::Real, y2::Real, args...; kwargs...)
        f = convert(Function, ex)
        Gadfly.plot((x,y) -> convert(Float64, f(x,y)), x1, x2, y1, y2, args...; kwargs...)
    end

        eval(Expr(:export, :parametricplot))
        eval(Expr(:export, :contour))
end

## Plots interface
Requires.@require Plots begin

    import Plots.plotter!
    plotter!(:pyplot) # default, not :gadfly



function _plot(fn::Function, ex::SymbolicObject, a, b; kwargs...) 
    vars = free_symbols(ex)
    if length(vars) == 1
        f(x) = N(subs(ex, vars[1], x))
        fn(f, a, b; kwargs...)
    elseif length(vars) == 2
        error("Expression to plot may have only one free variable")
    end
end



function _plot{T<:SymbolicObject}(fn::Function, exs::Vector{T}, a, b; kwargs...) 
    vars = free_symbols(maximum(exs))
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
    vars = free_symbols(max(exs[1], exs[2]))
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




end

end
