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
## that doesn't depend on the backend plotting packages, so we
## use the `Plots` package.
##
##
## plot(::Sym, a, b)          2d-line plot of function
## plot(Vector{Sym}, a, b)    plot of expressions over same axis
## plot(::Sym, ::Sym, a, b)   parametric plot of two expressions over [a,b]
## plot(xs, ys, ex::Sym)      contour plot of expression over regions xs x ys
##
## To this we add a light interface for explicitness:
##
## parametricplot(ex1, ex2, [ex3], a, b) parametric plot
## contourplot(ex, (xvar, a, b), (yvar, c,d)) contour plot
##
##
## The PyPlot package adds 3d plotting, quiver (vectorplot), and implicit plots

## TODO: should `add_arrow`  be `quiver!`? `quiver` be `vectorplot`? `plot_vectorfield`? ...

using Requires ## for conditional `@require`ing of packages

"""

Plotting of symbolic objects.

The `Plots` package provide a uniform interface to many of `Julia`'s
plotting packages. `SymPy` extends the methods for functions to plot
symbolic objects.

The basic goal is that when `Plots` provides an interface for function
objects, this package extends the interface to symbolic
expressions. The convenience comes at a cost -- the plots are
slow to render. (There is a conversion from SymPy value to numeric values that is slow.)

If no backend plotting package is loaded directly, then the `plot` and
`plot!` methods of `Plots` allow for direct plotting of `SymPy`
objects.

In particular:


* `plot(ex::Sym, a, b; kwargs...)` will plot a function evaluating `ex` over [a,b]

Example. Here we use the default backend for `Plots` to make a plot:

```
@vars x
plot(x^2 - 2x, 0, 4)
```

The backend could be switched out with a call like, `backend(:pyplot)`, say.

* `plot(exs::Vector{Sym}, a, b; kwargs...)` will plot the functions evaluating `exs` over [a,b]

Example:

```
@vars x
plot([sin(x), cos(x)], 0, 2pi)
```

* `plot(ex1, ex2, a, b; kwargs...)` will plot the two expressions in a parametric plot over the interval `[a,b]`.   

Example:

```
@vars x
plot(sin(2x), cos(3x), 0, 4pi) ## also 
```

For explicitness, we provide `parametricplot(ex1, ex2, a, b)` as an alteranative.


* `plot(xs, ys, expression)` will make a contour plot (for many backends).

```
@vars x y
plot(linspace(0,5), linspace(0,5), x*y)
```

For explicitness, we provide `contourplot(ex::Sym, (xvar, a, b), (yvar, c, d))` as an alternative:

```
contourplot(x^2 - y^2, (x,-5,5), (y,-5,5)) # default is [-5,5] x [-5,5]
```


----

If the `PyPlot` backend is used (as with `backend(:pyplot)`), then there are additional methods added. 

* `plot(ex1, ex2, ex3, a, b; kwargs)` produces a 3D parametric plot over `[a,b]` (also `parametricplot`)

```
@vars x
plot(sin(x), cos(x), x, 0, 4pi)  
```

* `contour3D(ex, (x,x0, x1), (y,y0, y1); kwargs...)` wiil plot a contour plot in  3D over the region.


```
@vars x y z
contour3D( x^2 - y^2) # default is [-5,5] x [-5,5]
```

* `quiver(Vector{Sym})` produces a plot of a vector field. The alias
  `vectorplot` is provided, though this may change if vector-field
  plots get added to the `Plots` package.

```
quiver([-y, x], (x, -5,5), (y, -5,5 ))  ## same as vectorplot([-y,x]) using the default range
```

* `plot_surface(ex, (x,-5,5), (y,-5,5); kwargs)` Plot  surface of z=ex(x,y) over region

```
@vars x y
plot_surface(25 - (x^2 + y^2))
```

The SymPy plotting module in Python also adds the following two features to Matplotlib. As such, these are not in `PyPlot`, but rather the `SymPy` namespace.

* `plot_parametric_surface(exs::Tuple, (uvar,a0,b0), (vvar,a1,b1);  kwargs)`

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

(The SymPy name for this function is `plot3d_parametric_surface`, we have dropped the "`3d`" part.)

* `plot_implicit(pred, [(xvar,x0,x1), (yvar, y0,y1)]; kwargs...)` Make
  implicit plot of region. To specify the region use `(variable, a,b)`
  for the two variables.


To create conditions on the variable, the functions `Lt`, `Le`, `Eq`,
`Ge`, and `Gt` can be used. For infix notation, unicode operators can
be used: `\ll<tab>`, `\le<tab>`, `\Equal<tab>`, `\ge<tab>`, and
`\gg<tab>`. To combine terms, the unicode `\vee<tab>` (for "or"),
`\wedge<tab>` (for "and") can be used

Examples:

```
@vars x y
plot_implicit(sin(x+y) - cos(x^2 + y^2), (x, -5,5), (y, -5, 5))
plot_implicit(x ≫ y)  # over default region of [-5,5] x [-5,5]
f(x,y) = x^2 + y^2
plot_implicit((f(x,y) ≪ 5) ∧ (f(x,y) ≥ 2), (x,-5,5), (y,-5,5))
```

### Note:

These methods are added *after* the `PyPlot` package is loaded.

This can cause confusion:

* If `PyPlot` is loaded through `Plots` (by specifiying `backend(:pyplot)`)
  then `PyPlot` is not loaded until after the first `plot` call. The
  extended features for `PyPlot` are added then (at runtime), so can't
  be accessed until `PyPlot` is loaded.

* If `PyPlot` is loaded with `using PyPlot`, then the `plot` command
  is ambiguous with that of `PyPlot` and must be qualified
  (e.g. `SymPy.plot(...)`). However, the extended methods can be
  accessed directly. This would also be the case were another package
  with a `plot` method loaded, such as `Gadfly` or `Winston`.


Underneath these plotting functions is a fairly simple translation
from a symbolic expression to a function. The `convert(Function, ex)`
pattern can be used for scalar function. (Or if using version `v0.4`
or higher, `x->N(ex(x))`.) As well, numeric values can be generated
explicitly via a pattern akin to `Float64[ex(x) for x in xs]`.

"""
sympy_plotting = nothing
export sympy_plotting

## Our alternatives
"""

Create a parametric plot of the expressions over the interval `[a,b]`.

(A more explicit call of the type `plot(ex1, ex2, a, b)`.
"""
function parametricplot(ex1::Sym, ex2::Sym, a::Real, b::Real, args...; kwargs...)
    Plots.plot(ex1, ex2, a,b, args...; kwargs...)
end
export(parametricplot)

"""

Create a contour plot of the expression over the indicated region. The
region is specified as a tuple `(a,b)` or `(x,a,b)`. The former has
the associated variable inferred using the ordering of `free_symbols`.

(This is a more explicit form of interface provided by `Plot`:
`plot(xs, ys, ex::Sym)`.)

"""
function contourplot(ex::Sym,
                 xvar=(-5.0, 5.0),
                 yvar=(-5.0, 5.0),
                 args...;
                 n::Int=50,
                 kwargs...)
    U,V,xs,ys = _find_us_vs(ex, xvar, yvar, n)
    plot(xs, ys, ex, args...; kwargs...)
end
export(contourplot)

### Plug into Plots interface. Where there is something defined for Functions we
### define for symbolic expressions
## Additions to Plots so that expressions are treated like functions

typealias SymOrSyms @compat(Union{Sym, Plots.AVec{Sym}})
Plots.convertToAnyVector(ex::Sym; kw...) = Any[ex], nothing
Plots.computeY(xs, ex::Sym) = Float64[ex(x) for x in xs]

mapSymOrSyms(f::Sym, u::Plots.AVec) = Float64[f(x) for x in u]
mapSymOrSyms(fs::Plots.AVec{Sym}, u::Plots.AVec) = [mapSymOrSyms(f, u) for f in fs]

## # contours or surfaces... 
function Plots.createKWargsList{T<:Real,S<:Real}(plt::Plots.PlottingObject, x::Plots.AVec{T}, y::Plots.AVec{S}, zf::Sym; kw...)
    # only allow sorted x/y for now
    # TODO: auto sort x/y/z properly
    @assert x == sort(x)
    @assert y == sort(y)
    surface = Float64[zf(xi, yi) for xi in x, yi in y]
    Plots.createKWargsList(plt, x, y, surface; kw...)  # passes it to the zmat version
end


# list of expressions
function Plots.createKWargsList(plt::Plots.PlottingObject, f::SymOrSyms, x; kw...)
    @assert !(typeof(x) <: Sym)  # otherwise we'd hit infinite recursion here
    Plots.createKWargsList(plt, x, f; kw...)
end

# special handling... xmin/xmax with function(s)
function Plots.createKWargsList(plt::Plots.PlottingObject, f::SymOrSyms, xmin::Real, xmax::Real; kw...)
    width = plt.initargs[:size][1]
    x = collect(linspace(xmin, xmax, width))  # we don't need more than the width
    Plots.createKWargsList(plt, x, f; kw...)
end

# special handling... xmin/xmax with parametric function(s)
Plots.createKWargsList{T<:Real}(plt::Plots.PlottingObject, fx::Sym, fy::Sym, u::Plots.AVec{T}; kw...) =
    Plots.createKWargsList(plt, mapSymOrSyms(fx, u), mapSymOrSyms(fy, u); kw...)
    

Plots.createKWargsList{T<:Real}(plt::Plots.PlottingObject, u::Plots.AVec{T}, fx::SymOrSyms, fy::SymOrSyms; kw...) =
    Plots.createKWargsList(plt, mapSymOrSyms(fx, u), mapSymOrSyms(fy, u); kw...)


Plots.createKWargsList(plt::Plots.PlottingObject, fx::Sym, fy::Sym, umin::Real, umax::Real, numPoints::Int = 1000; kw...) =
    Plots.createKWargsList(plt, fx, fy, linspace(umin, umax, numPoints); kw...)

###


## Helper functions
## prepare parametic takes exs, [t0,t1] and returns [xs, ys] or [xs, ys, zs]
function _prepare_parametric(exs, t0, t1, n=250)
    vars = free_symbols(exs)
    nexs = length(exs)
    (nexs==2) | (nexs==3) || throw(DimensionMismatch("parametric plot requires the initial tuple to have 2 or 3 variables"))
    length(vars) == 1 ||  error("parametric plot allows only one free variable")

    ts = linspace(t0, t1, n)
    [Float64[convert(Float64, convert(Function, exs[i])(t)) for t in ts] for i in 1:nexs] # [[xs...], [ys...], [zs...]]
end

## parse (ex, ([x], a, b), ([y], c,d)) into variables x,y and ranges xs, ys.
function _find_us_vs(ex, xvar, yvar, n=100)
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


######################################    
## Must put Requires.require outside of compilation
function init_plot()

    ## PyPlot
    Requires.@require PyPlot begin

        info("""Loading additional PyPlot commands for graphing for SymPy objects:
quiver, parametricplot (for 3D), contour3D, plot_surface, plot_parametric_surface, and plot_implicit.
See ?sympy_plotting for some more details
""")
        function parametricplot(ex1::Sym, ex2::Sym, ex3::Sym, a::Real, b::Real, args...; n=250, kwargs...)
            out = _prepare_parametric([ex1,ex2,ex3], a, b, n)
            PyPlot.plot3D(out..., args...; kwargs...)
        end

        ## Ideally we would name `contourplot` just `contour`, but for a few reasons:
        ## * adding the suffix "plot" is consistent with `parametricplot` and `vectorplot`
        ## * if the user calls in `PyPlot` via `using`, there is no naming conflict.
        ## here we add in the `contour` name for convenience when the user is using `PyPlot`.
        eval(Expr(:import, :PyPlot, :contour))
        function contour(ex::Sym, xvar=(-5.0, 5.0),
                         yvar=(-5.0, 5.0),
                         args...;
                         n::Int=25,
                         kwargs...)
            contourplot(ex, xvar,yvar, args...;n=n, kwargs...)
        end

        
        ## quiver ,,,http://matplotlib.org/examples/pylab_examples/quiver_demo.html
        ## We add a plot of vectors (vectorplot). Hopefully this will be in `Plots` one
        ## day
        eval(Expr(:import, :PyPlot, :quiver))
        function quiver(exs::Vector{Sym},
                               xvar=(-5.0, 5.0),
                               yvar=(-5.0, 5.0),
                               args...;
                               n::Int=25,
                               kwargs...) 
                                   
            length(exs) == 2 || throw(DimensionMismatch("vector of symbolic objects must have length 2"))
            for ex in exs
                nvars = length(free_symbols(ex))
                nvars <= 2 || throw(DimensionMismatch("Expression has $nvars, expecting 2 for a quiver plot"))
            end
            
            U,V,xs,ys = _find_us_vs(exs, xvar, yvar, n)
            
            us = Float64[subs(exs[1], (U,x), (V,y)) for x in xs, y in ys]
            vs = Float64[subs(exs[2], (U,x), (V,y)) for x in xs, y in ys]
            
            PyPlot.quiver(xs, ys, us, vs, args...; kwargs...)
        end
        global  vectorplot = PyPlot.quiver
        
        ## XXX Need an interface decision here...
        global add_arrow(p::Vector, v::Vector, args...; kwargs...) = begin
            n = length(p)
            if n == 2
                PyPlot.arrow(p..., v...; kwargs...)
            elseif n==3
                out = [hcat(p,p+v)'[:,i] for i in 1:n]
                PyPlot.plot3D(out..., args...; kwargs...)
            end
        end

        
        ## 3D contour plot
        eval(Expr(:import, :PyPlot, :contour3D))        
        function contour3D(ex::Sym,
                                  xvar=(-5.0, 5.0),
                                  yvar=(-5.0, 5.0),
                                  args...;
                                  n::Int=50,
                                  kwargs...)


            U,V,xs,ys = _find_us_vs(ex, xvar, yvar, n)                        

            zs = Float64[subs(ex, (U,x), (V,y)) for x in xs, y in ys]
            PyPlot.contour3D(xs, ys, zs, args...; kwargs...)
        end
        
        ## surface plot
        eval(Expr(:import, :PyPlot, :plot_surface))        
        function plot_surface(ex::Sym,
                                     xvar=(-5.0, 5.0),
                                     yvar=(-5.0, 5.0),
                                     args...;
                                     n::Int=35,
                                     kwargs...)

            vars = free_symbols(ex)
            length(vars) == 2 || throw(DimensionMismatch("Expression has wrong number of variables. Expecting 2 for a surface plot"))
            
            U,V,xs,ys = _find_us_vs(ex, xvar, yvar, n)        

         
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
            
            U,V,us,vs = _find_us_vs(exs, xvar, yvar, n)        

            
            xs = Float64[subs(exs[1], (U,u), (V,v)) for u in us, v in vs]
            ys = Float64[subs(exs[2], (U,u), (V,v)) for u in us, v in vs]
            zs = Float64[subs(exs[3], (U,u), (V,v)) for u in us, v in vs]
            
            
            PyPlot.plot_surface(xs, ys, zs, args...; kwargs...)
        end

        ## Implict equations plot
        plot_implicit(ex, args...; kwargs...) = sympy[:plotting][:plot_implicit](ex.x, project(args)...;  [(k,project(v)) for (k,v) in kwargs]...)

        eval(Expr(:export, :contour3D))
        eval(Expr(:export, :vectorplot))
        eval(Expr(:export, :quiver))
        eval(Expr(:export, :add_arrow))
        eval(Expr(:export, :plot_surface))
        eval(Expr(:export, :plot_parametric_surface))
        eval(Expr(:export, :plot_implicit))
        
    end

end
