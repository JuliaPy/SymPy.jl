## add plotting commands for various packages (Winston, PyPlot, Gadfly)

## SymPy (http://docs.sympy.org/latest/modules/plotting.html ) has its plotting module that provides
## * plot: Plots 2D line plots.
## * plot_parametric: Plots 2D parametric plots.
## * plot3d_parametric_line: Plots 3D line plots, defined by a parameter.
## * plot3d: Plots 3D plots of functions in two variables.
## * plot3d_parametric_surface: Plots 3D parametric surface plots.
## * plot_implicit: Plots 2D implicit and region plots.
##
## Our goal here is to give this a Julia interface
## that doesn't depend on the backend plotting packages
##
## The `Plots` package will provide that, but right now it doesn't support
## all the graphics here, including `contour`, `vectorplot`, `plot_implicit` and the 3D plots.
## Currently those are supported by the PyPlot packages.
##
## When they are supported, the names of the functions below will change to match
## those in the `Plots` package.
##

## The plotting package determines what is available. Implementations:
#
#                                PyPlot     Gadfly    Winston    Plots
# plot(::Sym)                     ✓           ✓          ✓        ✓
# plot(::Vector{Sym})             .           ✓          .         ✓
# plot(::Tuple{Sym})              ✓           ✓(2D)      ✓(2D)    ✓ (2D)  (aka parametricplot)
# contour(::Vector{Sym})          ✓           ✓          .        .
# vectorplot(::Vector{Sym})       ✓           .          .        .
# contour3D(::Vector{Sym})        ✓           .          .        .
# plot_surface(::Vector{Sym})     ✓           .          .        .
# plot_parametric_surface(::Vector{Sym})     ✓           .          .        .
# add_arrow(p,v)                  ✓           .          .        .
# text...                         .           .          .        .

## [DONE] TODO: use type `(var, a,b)` when region is specified
## [DONE] TODO: use n, not nx and ny to speciy no. of points.
## TODO: deprecate `parametricplot`...
## TODO: should `add_arrow`  be `vectorplot!`?

using Requires ## for conditional `@require`ing of packages

"""

Plotting of symbolic objects.

`SymPy` uses the `Plots` package to provide a uniform interface to
many of `Julia`'s plotting packages. The default one used is
`Gadfly`. These methods are extended to plot symbolic objects:


* `plot(ex::Sym, a, b; kwargs...)` will plot a function evaluating `ex` over [a,b]

Example:

```
@vars x
plot(x^2 - 2x, 0, 4)
```

* `plot(exs::Vector{Sym}, a, b; kwargs...)` will plot the functions evaluating `exs` over [a,b]

Example:

```
@vars x
plot([sin(x), cos(x)], 0, 2pi)
```

* `plot((ex1, ex2), a, b; kwargs...)` will plot the two expressions in a parametric plot over the interval [a,b]

Example:

```
@vars x
plot((sin(2x), cos(3x)), 0, 4pi)
```

* `plot((ex1, ex2, ex3), a, b; kwargs)` will create a 3D parametric plot over `[a,b]`

```
@vars x
plot((sin(x), cos(x), x), 0, 4pi)  ## PyPlot backend only
```

* `parametricplot(ex1, ex2, [ex3], a, b, args...;kwargs...)` is just `plot((exs...), a, b)` Alternate form for `plot((ex1,ex2), a,b)`.

* `contour(ex, (x,x0, x1), (y,y0, y1), args...; kwargs...)` wiil plot a contour plot over the region specified with tuples. The variables `x` and `y` are the free variables in `ex`. The default region is `[-5,5]x[-5,5]`.

Example:

```
@vars x y
contour(x^2 - y^2, (x,-5,5), (y,-5,5)) # default is [-5,5] x [-5,5] (PyPlot, Gadfly only)
```

* `contour3D(ex, (x,x0, x1), (y,y0, y1); kwargs...)` wiil plot a 3D contour plot over the region


```
@vars x y z
contour3D( x^2 - y^2) # default is [-5,5] x [-5,5] (PyPlot only)
```

* `plot_surface(ex, (x,-5,5), (y,-5,5); kwargs)` Plot  surface of z=ex(x,y) over region

```
@vars x y
plot_surface(25 - (x^2 + y^2))
```

* `plot_parametric_surface(exs::Tuple, (uvar,a0,b0), (vvar,a1,b1);  kwargs)` (PyPlot only)

Plot the parametrically defined surface `[exs[1](u,v), exs[2](u,v), exs[3](u,v)]` over `[a0,a1] x
[b0,b1]`. The specification of the variables uses a tuple of the form
`(Sym, Real, Real)` following the style of SymPy in `integrate`, say,
where disambiguation of variable names is needed.

```
@vars theta, phi
r = 1
plot_parametric_surface((r*sin(theta)*sin(phi), r*sin(theta)*cos(phi), r*cos(theta)),
                        (theta, 0, pi), (phi, 0, pi/2))
```


* `plot_implicit(pred, [(xvar,x0,x1), (yvar, y0,y1)]; kwargs...)` Make
  implicit plot of region. To specify the region use `(variable, a,b)`
  for the two variables.


To create conditions on the variable, the functions `Lt`, `Le`, `Eq`,
`Ge`, and `Gt` can be used. For infix notation, unicode operators can
be used: `\ll<tab>`, `\le<tab>`, `\Equal<tab>`, `\ge<tab>`, and
`\gg<tab>`.

To combine terms, the unicode `\vee<tab>` (for "or"), `\wedge<tab>` (for "and") can be used

(Warning, if using in `IJulia`, must be used **after** loading `PyPlot`.)


Examples:

```
@vars x y
plot_implicit(sin(x+y) - cos(x^2 + y^2), (x, -5,5), (y, -5, 5))
plot_implicit(x ≫ y)  # over default region of [-5,5] x [-5,5]
f(x,y) = x^2 + y^2
plot_implicit((f(x,y) ≪ 5) ∧ (f(x,y) ≥ 2), (x,-5,5), (y,-5,5))
```

"""


## Helper function
## prepare parametic takes exs, [t0,t1] and returns [xs, ys] or [xs, ys, zs]
function prepare_parametric(exs, t0, t1, n=250)
    vars = free_symbols(exs)
    nexs = length(exs)
    (nexs==2) | (nexs==3) || throw(DimensionMismatch("parametric plot requires the initial tuple to have 2 or 3 variables"))
    length(vars) == 1 ||  error("parametric plot allows only one free variable")

    ts = linspace(t0, t1, n)
    [Float64[convert(Float64, convert(Function, exs[i])(t)) for t in ts] for i in 1:nexs] # [[xs...], [ys...], [zs...]]
end

function find_us_vs(ex, xvar, yvar, n=100)
    vars = free_symbols(ex)
    if length(xvar) == 2
        U = vars[1]
        x1,x2 = xvar
    else
        U, x1, x2 = xvar
    end
    if length(yvar) == 2
        V = vars[2]
        y1,y2 = yvar
    else
        V, y1, y2 = yvar
    end
    xs = linspace(x1, x2, n)
    ys = linspace(y1, y2, n)
    U, V, xs, ys
end


## plot(ex, a, b)
function _plot(fn::Function, ex::SymbolicObject, a, b; kwargs...) 
    vars = free_symbols(ex)
    if length(vars) == 1
        fn(convert(ScalarFunction, ex), a, b; kwargs...)
    elseif length(vars) == 2
        error("Expression to plot may have only one free variable")
    end
end


## plot([exs], a, b)
function _plot{T<:SymbolicObject}(fn::Function, exs::Vector{T}, a, b; kwargs...) 
    vars = free_symbols(exs)
    if length(vars) == 1
        fns = map(ex -> convert(ScalarFunction, ex), exs)
        fn(fns, a, b; kwargs...)
    elseif length(vars) == 2
        error("Expressions to plot may have only one common free variable")
    end
end

# Parametric plots
## 2d parametric plot plot((ex1, ex2), a, b)
function _plot{T<:SymbolicObject, S<:SymbolicObject}(fn::Function, exs::(@compat Tuple{T,S}), t0::Real, t1::Real, args...; kwargs...)
    vars = free_symbols(exs)
    if length(vars) == 1
        fn(map(ex->convert(ScalarFunction, ex), exs)..., t0, t1, args...; kwargs...)
    else
        error("Parametric plot may have only one free variable")
    end
end

## 3D parametric plot
function _plot{T<:SymbolicObject, S<:SymbolicObject, R<:SymbolicObject}(fn::Function, exs::(@compat Tuple{T,S,R}), t0::Real, t1::Real, args...; kwargs...)
    vars = free_symbols(exs)
    if length(vars) == 1
        fn(map(ex->convert(ScalarFunction, ex), exs)..., t0, t1, args...; kwargs...)
    else
        error("Parametric plot may have only one free variable")
    end
end




    
## Must put Requires.require outside of compilation
function init_plot()

    ## Plots interface
    Requires.@require Plots begin
        info("Loading Plots interface for SymPy objects")

        Plots.plot(ex::SymbolicObject, a, b; kwargs...) = _plot(Plots.plot, ex, a, b; kwargs...)
        Plots.plot!(ex::SymbolicObject, a, b; kwargs...) = _plot(Plots.plot!, ex, a, b; kwargs...)
        
        Plots.plot{T<:SymbolicObject}(exs::Vector{T}, a, b; kwargs...) = _plot(Plots.plot, exs, a, b; kwargs...)
        Plots.plot!{T<:SymbolicObject}(exs::Vector{T}, a, b; kwargs...) = _plot(Plots.plot!, exs, a, b; kwargs...)
        
        Plots.plot{T<:SymbolicObject, S<:SymbolicObject}(exs::(@compat Tuple{T,S}), t0::Real, t1::Real, args...; kwargs...) =
            _plot(Plots.plot, exs, t0, t1, args...; kwargs...)
        Plots.plot!{T<:SymbolicObject, S<:SymbolicObject}(exs::(@compat Tuple{T,S}), t0::Real, t1::Real, args...; kwargs...) =
            _plot(Plots.plot!, exs, t0, t1, args...; kwargs...)
        
        Plots.plot{T<:SymbolicObject, S<:SymbolicObject}(ex1::T, ex2::S, t0::Real, t1::Real, args...; kwargs...) =
            _plot(Plots.plot, [ex1, ex2], t0, t1, args...; kwargs...)
        Plots.plot!{T<:SymbolicObject, S<:SymbolicObject}(ex1::T, ex2::S, t0::Real, t1::Real, args...; kwargs...) =
            _plot(Plots.plot!, [ex1, ex2], t0, t1, args...; kwargs...)

        function parametricplot(ex1::Sym, ex2::Sym, a::Real, b::Real, args...; kwargs...)
            Plots.plot((ex1, ex2), a,b, args...; kwargs...)
        end
        function parametricplot(ex1::Sym, ex2::Sym, ex3::Sym, a::Real, b::Real, args...; kwargs...)
            Plots.plot((ex1, ex2, ex3), a,b, args...; kwargs...)
        end
        eval(Expr(:export, :parametricplot))
    end
        
    ## PyPlot
    Requires.@require PyPlot begin
        info("Loading PyPlot graphing for SymPy objects")
        
        function PyPlot.plot(ex::Sym, a::Real, b::Real, args...; n::Int=250, kwargs...)
            vars = free_symbols(ex)
            length(vars) > 1 && error("plot of expression can only have 1 free variables")

            f = convert(Function, ex)
            xs = linspace(a,b, n)
            ys = map(x->convert(Float64,f(x)), xs)
            PyPlot.plot(xs, ys, args...; kwargs...)
        end
        
        function PyPlot.plot(exs::Array{Sym,1},a::Real,b::Real, args...; n::Int=250,kwargs...)
            xs = linspace(a,b,n)
            fs = map(ex->convert(Function, ex), exs)
            for i in 1:length(fs)
                PyPlot.plot(xs, map(x->N(fs[i](x)),xs))
            end
        end
        
        ## ## Parametric plots use notation plot((ex1,ex2, [ex3]), t0, t1, args..., kwargs...)
        function PyPlot.plot{T<:Sym, S<:Sym}(exs::@compat(Tuple{T,S}), t0::Real, t1::Real, args...; n::Int=250, kwargs...)
            out = prepare_parametric(exs, t0, t1, n)
            PyPlot.plot(out..., args...; kwargs...)
            end
        function PyPlot.plot{T<:Sym, S<:Sym,R<:Sym}(exs::(@compat Tuple{T,S,R}), t0::Real, t1::Real, args...; n::Int=250, kwargs...)
            out = prepare_parametric(exs, t0, t1,n)
            PyPlot.plot3D(out..., args...; kwargs...)
        end

        ## quiver ,,,http://matplotlib.org/examples/pylab_examples/quiver_demo.html
        global  vectorplot(exs::Vector{Sym},
                           xvar=(-5.0, 5.0),
                           yvar=(-5.0, 5.0),
                           args...;
                           n::Int=25,
                           kwargs...) = begin
                                   
                               length(exs) == 2 || throw(DimensionMismatch("vector of symbolic objects must have length 2"))
                               for ex in exs
                                   nvars = length(free_symbols(ex))
                                   nvars <= 2 || throw(DimensionMismatch("Expression has $nvars, expecting 2 for a quiver plot"))
                               end

                               U,V,xs,ys = find_us_vs(exs, xvar, yvar, n)

                               us = Float64[subs(exs[1], (U,x), (V,y)) for x in xs, y in ys]
                               vs = Float64[subs(exs[2], (U,x), (V,y)) for x in xs, y in ys]
                               
                               PyPlot.quiver(xs, ys, us, vs, args...; kwargs...)
                           end


        ## Need an interface decision here...
        global add_arrow(p::Vector, v::Vector, args...; kwargs...) = begin
            n = length(p)
            if n == 2
                PyPlot.arrow(p..., v...; kwargs...)
            elseif n==3
                out = [hcat(p,p+v)'[:,i] for i in 1:n]
                PyPlot.plot3D(out..., args...; kwargs...)
            end
        end

        ## Contour plot
        function PyPlot.contour(ex::Sym,
                                xvar=(-5.0, 5.0),
                                yvar=(-5.0, 5.0),
                                args...;
                                n::Int=50,
                                kwargs...)

            U,V,xs,ys = find_us_vs(ex, xvar, yvar, n)            
            
            zs = Float64[subs(ex, (U,x), (V,y)) for x in xs, y in ys]
            PyPlot.contour(xs, ys, zs, args...; kwargs...)
        end
        
        ## 3D contour plot
        function PyPlot.contour3D(ex::Sym,
                                  xvar=(-5.0, 5.0),
                                  yvar=(-5.0, 5.0),
                                  args...;
                                  n::Int=50,
                                  kwargs...)


            U,V,xs,ys = find_us_vs(ex, xvar, yvar, n)                        

            zs = Float64[subs(ex, (U,x), (V,y)) for x in xs, y in ys]
            PyPlot.contour3D(xs, ys, zs, args...; kwargs...)
        end
        
        ## surface plot
        function PyPlot.plot_surface(ex::Sym,
                                     xvar=(-5.0, 5.0),
                                     yvar=(-5.0, 5.0),
                                     args...;
                                     n::Int=35,
                                     kwargs...)

            vars = free_symbols(ex)
            length(vars) == 2 || throw(DimensionMismatch("Expression has wrong number of variables. Expecting 2 for a surface plot"))

            U,V,xs,ys = find_us_vs(ex, xvar, yvar, n)        

         
            zs = Float64[subs(ex, (U,x), (V,y)) for x in xs, y in ys]
            PyPlot.plot_surface(xs, ys, zs, args...; kwargs...)
        end
        
        ## surface plot xvar = Tuple(Sym, Real, Real)
        ##
        function plot_parametric_surface(exs::(@compat Tuple{Sym,Sym,Sym}),
                                         xvar=(-5.0, 5.0),
                                         yvar=(-5.0, 5.0),
                                         args...;
                                         n::Int=25, # really small, as otherwise this takes forever to plot
                                         kwargs...)

            vars = free_symbols(exs)

            nvars = length(vars)
            nvars == 2 || throw(DimensionMismatch("Expression has $nvars, expecting 2 for a surface plot"))

            U,V,us,vs = find_us_vs(exs, xvar, yvar, n)        


            xs = Float64[subs(exs[1], (U,u), (V,v)) for u in us, v in vs]
            ys = Float64[subs(exs[2], (U,u), (V,v)) for u in us, v in vs]
            zs = Float64[subs(exs[3], (U,u), (V,v)) for u in us, v in vs]
            
            
            PyPlot.plot_surface(xs, ys, zs, args...; kwargs...)
        end
 
        plot_implicit(ex, args...; kwargs...) = sympy[:plotting][:plot_implicit](ex.x, project(args)...;  [(k,project(v)) for (k,v) in kwargs]...)
        
        function parametricplot(ex1::Sym, ex2::Sym, a::Real, b::Real, args...; kwargs...)
            PyPlot.plot((ex1, ex2), a,b, args...; kwargs...)
        end
        function parametricplot(ex1::Sym, ex2::Sym, ex3::Sym, a::Real, b::Real, args...; kwargs...)
            PyPlot.plot((ex1, ex2, ex3), a,b, args...; kwargs...)
        end
        eval(Expr(:export, :parametricplot))

        eval(Expr(:export, :vectorplot))
        eval(Expr(:export, :add_arrow))
        eval(Expr(:export, :plot_surface))
        eval(Expr(:export, :plot_parametric_surface))
        eval(Expr(:export, :plot_implicit))
        
    end

    ## Gadfly plots
    Requires.@require Gadfly begin
        
        info("Adding Gadfly plotting for SymPy Objects")
        typealias GadflyType @compat Union{Gadfly.Element,DataType,Gadfly.Theme,Function}
        
        Gadfly.plot(ex::Sym, a::Real, b::Real, args...; kwargs...) =
            _plot(Gadfly.plot, ex, a, b, args...; kwargs...)
        
        Gadfly.plot(exs::Array{Sym,1},a::Real,b::Real,args::GadflyType...; kwargs...) =
        _plot(Gadfly.plot, exs, a, b, args...; kwargs...)
        
        function Gadfly.plot{T<:Sym, S<:Sym}(exs::(@compat Tuple{T,S}), a::Real, b::Real, args...; n=250, kwargs...)
            ts = linspace(a, b, n)
            var = free_symbols(exs)[1]
            xs = Float64[subs(exs[1], (var,t)) for t in ts]
            ys = Float64[subs(exs[2], (var,t)) for t in ts]
            Gadfly.plot(x=xs, y=ys, args...; kwargs..., Geom.line(preserve_order=true))
        end

        function Gadfly.plot{T<:Sym, S<:Sym, R<: Sym}(exs::(@compat Tuple{T,S,R}), t0::Real, t1::Real, args...; kwargs...)
            error("No 3D parametric line plot is possible with Gadfly")
        end

            
        function contour(ex::Sym, xvar=(-5.0, 5.0), yvar=(-5.0, 5.0), args...; kwargs...)
            U,V,xs,ys = find_us_vs(ex, xvar, yvar)

            f(x,y) = convert(Float64, subs(ex, (U,x), (V,y)))

            Gadfly.plot(f,extrema(xs)..., extrema(ys)..., args...; kwargs...)
        end


        function parametricplot(ex1::Sym, ex2::Sym, a::Real, b::Real, args...; kwargs...)
            Gadfly.plot((ex1, ex2), a,b, args...; kwargs...)
        end
        function parametricplot(ex1::Sym, ex2::Sym, ex3::Sym, a::Real, b::Real, args...; kwargs...)
            Gadfly.plot((ex1, ex2, ex3), a,b, args...; kwargs...)
        end
        eval(Expr(:export, :parametricplot))

        eval(Expr(:export, :contour))
    end

    ## Winston plotting
    Requires.@require Winston begin
        info("Adding Winston plotting for SymPy objects")
        
        Winston.plot(ex::Sym, a, b, args...; kwargs...) =
            _plot(Winston.plot, ex, a, b, args...; kwargs...)

        Winston.oplot(ex::Sym, args...; kwargs...) = 
            _plot(Winston.oplot, ex, a, b, args...; kwargs...)
        
        #Winston.plot(exs::Array{Sym,1},a::Real,b::Real,args...; kwargs...) = 
        #    _plot(Winston.plot, exs, a, b, args...; kwargs...)
        
        Winston.plot{T<:Sym, S<:Sym}(exs::(@compat Tuple{T,S}), t0::Real, t1::Real, args...; kwargs...) =
            _plot(Winston.plot, exs, a, b, args...; kwargs...)
        
        function Winston.plot{T<:Sym, S<:Sym,R<:Sym}(exs::(@compat Tuple{T,S,R}), t0::Real, t1::Real, args...; kwargs...)
            error("No 3D parametric plot with Winston")
        end


        global add_arrow(p::Vector, v::Vector) = begin
            n = length(p)
            p,v = map(x -> convert(Float64, x), p), map(x -> convert(Float64, x), v)
            n == 2 || error("Winston is only 2 dimensional")
            Winston.oplot([p[1], p[1] + v[1]], [p[2], p[2] + v[2]])
        end

        function parametricplot(ex1::Sym, ex2::Sym, a::Real, b::Real, args...; kwargs...)
            Winston.plot((ex1, ex2), a,b, args...; kwargs...)
        end
        function parametricplot(ex1::Sym, ex2::Sym, ex3::Sym, a::Real, b::Real, args...; kwargs...)
            Winston.plot((ex1, ex2, ex3), a,b, args...; kwargs...)
        end

        eval(Expr(:export, :parametricplot))
        eval(Expr(:export, :add_arrow))
    end


end
