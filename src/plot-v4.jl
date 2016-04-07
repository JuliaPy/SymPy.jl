println("plot-v4")
## add plotting commands for various packages (Winston, PyPlot, Gadfly)
##
## Based on Plots.jl v"0.5.1+".  In particular, this assume Julia v"0.4+"
##
##
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
## parametricplot(ex1, ex2, [ex3], a, b) # parametric plot in 2 or 3d
## contourplot(ex, (xvar, a, b), (yvar, c,d))  # also Plots.contour(xs, ys, ex)
## plot_surface(ex, (xvar, a, b), (yvar, c,d)) # also Plots.surface(xs, ys, ex)
## plot_parametric_surface((ex1, ex2, ex3), (uvar, a, b), (cvar, c,d))
##
## we also add
## vectorfieldplot([ex1, ex2], (xvar, a, b), (yvar, a, b)) for a vector field plot
##
## The PyPlot package adds quiver (like vectorfieldplot), 3d contours, and implicit plots
##
## TODO: should `add_arrow`  be `quiver!`? 

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

The backend could be switched out with a call like, `pyplot()` or `backend(:pyplot)`, say.

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

For explicitness, we provide `parametricplot(ex1, ex2, a, b)` as an alternative.

For a few backends (those that support `:path3d`) a third symbolic
expression may be added to have a 3d parametric plot rendered:

```
parametricplot(sin(x), cos(x), x, 0, 4pi) # helix in 3d
```

* `vectorfieldplot([ex1, ex2], (x,x0,x1), (y,y0,y1), ...)` will create a vectorfield plot where for a grid of values `(x,y)`, a vector in the direction `[ex1, ex2]`, evaluated at `(x,y)`, will be plotted.

```
vectorfield([-y,x], (x, -5,5), (y, -5,5))
```


* `plot(xs, ys, expression)` will make a contour plot (for many backends).

```
@vars x y
plot(linspace(0,5), linspace(0,5), x*y)
```

For explicitness, we provide `contourplot(ex::Sym, (xvar, a, b), (yvar, c, d))` as an alternative:

```
contourplot(x^2 - y^2, (x,-5,5), (y,-5,5)) # default is [-5,5] x [-5,5]
```



* To plot the surface  `z=ex(x,y)` over a region we have `plot_surface(ex, (x,-5,5), (y,-5,5); kwargs...)`. For example,

```
@vars x y
plot_surface(25 - (x^2 + y^2), (x,0,5), (y,0,5)) # default region could have been dropped
```

The `Plots.surface` function (for a few backends) can also make sureface plots through the syntax:

`Plots.surface(xs, ys, expression)`

(The `surface` method is not exported, as `plot` is by `SymPy`, hence the qualification of the package. The syntax `plot(xs, ys, expression, linetype=:surf)` should also work.)



* `plot_parametric_surface(exs::Tuple, (uvar,a0,b0), (vvar,a1,b1);
  kwargs)` will make parametrically defined surface plots.

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


## PyPlot only

The SymPy plotting module in Python also adds the following two
features to Matplotlib. As such, these are not in `PyPlot`, but rather
the `SymPy` namespace.


----

If the `PyPlot` backend is used (as with `backend(:pyplot)`), then there are additional methods added. 

* `contour3D(ex, (x,x0, x1), (y,y0, y1); kwargs...)` will plot a contour plot in  3D over the region.


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

* If `PyPlot` is loaded through `Plots` (by specifying `backend(:pyplot)`)
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
function parametricplot(ex1::Sym, ex2::Sym, ex3::Sym, a::Real, b::Real, args...; n=250, kwargs...)
    ts = linspace(a, b,n)
    x = free_symbols([ex1, ex2, ex2])[1]
    xs, ys, zs = [mapsubs(ex,x,ts) for ex in [ex1, ex2, ex3]]
    Plots.path3d(xs, ys, z=zs, args...; kwargs...)
end
export(parametricplot)


"""

Vector plot. At each (x,y) on a grid, plot a "vector" of [fx(x,y), fy(x,y)], suitably scaled.

Example:
```
fx(x,y) = -y; fy(x,y) = x
vectorfieldplot(fx, fy)
```

We can plot a direction field of an ODE too.

```
F(y,x) = 1 - y/x   # solution to y'(x) = F(y(x),x) is C/x + x/2
fx(x,y) = 1; fy(x,y) = F(y,x)
vectorfieldplot(fx, fy, xlim=(1,5))
y(x) = 2/x + x/2
plot!(y, 1, 5, linewidth=3)
```

"""
function vectorfieldplot(fx::Function, fy::Function; xlim=(-5,5), ylim=(-5,5),  n::Int=15, kwargs...)

    x₀, x₁ = xlim
    y₀, y₁ = ylim
    Δx = (x₁ - x₀) / (n-1)
    Δy = (y₁ - y₀) / (n-1)

    xs = x₀:Δx:x₁
    ys = y₀:Δy:y₁
    p = plot(xlim=xlim, ylim=ylim, legend=false, kwargs...)

    mx, my = 0.0, 0.0

    ## two loops, first to identify scaling factor so
    ## lines stay within box of width Δx by Δy
    for x in xs, y in ys
        mx = max(mx, abs(fx(x,y)))
        my = max(my, abs(fy(x,y)))
    end

    ## we want all lines to be in the box, so we scale
    λ = .95 *  min(Δx/mx, Δy/my)
    
    for x in xs, y in ys
        plot!([x, x + λ * fx(x,y)], [y, y + λ * fy(x,y)])
    end
    
    p
end

"""

Vector field plot. At each point (x,y) on a grid, plots the vector specified by the expression.

The limits are passed in as tuples of the form `(a,b)` or `(x,a,b)`. The latter specifies the symbol. The former will assign the "`x`" symbol to be the first free symbol and the "`y`" variable the second.

Example
```
@vars x y
vectorfieldplot([-y, x], (x, -5, 5), (y, -5, 5))
```
"""
function vectorfieldplot(exs::Vector{Sym},
            xvar=(-5.0, 5.0),
            yvar=(-5.0, 5.0);
            n::Int=15,
            kwargs...) 
            
    length(exs) == 2 || throw(DimensionMismatch("vector of symbolic objects must have length 2"))
    for ex in exs
                nvars = length(free_symbols(ex))
        nvars <= 2 || throw(DimensionMismatch("Expression has $nvars, expecting 2 for a quiver plot"))
    end
    
    ## get variables, U, V, xlim, ylim
    vars = free_symbols(exs)
    if length(xvar) == 2
        U = vars[1]
        xlim = xvar
    else
        U, xlim = xvar[1], xvar[2:3]
    end
    if length(yvar) == 2
        V, ylim = vars[2], yvar
    else
        V, ylim = yvar[1], yvar[2:3]
    end

    fx(x,y) = convert(Float64, exs[1](U=>x, V=>y))
    fy(x,y) = convert(Float64, exs[2](U=>x, V=>y))

    vectorfieldplot(fx, fy; xlim=xlim, ylim=ylim, n=n, kwargs...)
end
export vectorfieldplot


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

## XXX Rename these?? surface?)
## surface plot. Uses surface()
"""

Make a surface plot defined by the expression of two variables. 


Example
```
plot_surface(x*y, (x,0,3), (y,0,2))
```

This is an *alternative* to the use of surface plots in Plots.
That is, the above is the same as:

```
xs = linspace(0,3, 35)
ys = linspace(0,2,35)
Plots.surface(xs, ys, x*y)
```

Eventually uses `Plots.plot(..., linetype=:surf)`, so surface plots must be defined for the backend in use.
"""
function plot_surface(ex::Sym,
                      xvar=(-5.0, 5.0),
                      yvar=(-5.0, 5.0),
                      args...;
                      n::Int=35,
                      kwargs...)
    
    vars = free_symbols(ex)
    length(vars) == 2 || throw(DimensionMismatch("Expression has wrong number of variables. Expecting 2 for a surface plot"))
    
    U,V,xs,ys = _find_us_vs(ex, xvar, yvar, n)        
    
    
    zs = mapsubs2(ex, U, xs, V,ys)
    Plots.surface(xs, ys, z=Surface(zs), args...; kwargs...)
end
export plot_surface


## surface plot xvar = Tuple(Sym, Real, Real)
##
"""

Render a parametrically defined surface plot.

Example:
```
@vars u, v
plot_parametric_surface((u*v,u-v,u+v), (u,0,1), (v,0,1))
```

Eventually uses `Plots.plot(..., linetype=:surf)`, so surface plots must be defined for the backend in use.
"""
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
    
    xs = mapsubs2(exs[1], U, us, V,vs)
    ys = mapsubs2(exs[2], U, us, V,vs)
    zs = mapsubs2(exs[3], U, us, V,vs)            
    
    Plots.surface(xs, ys, z=Surface(zs), args...; kwargs...)
end
export plot_parametric_surface


##################################################
### Plug into Plots interface. Where there is something defined for Functions we
### define for symbolic expressions
## Additions to Plots so that expressions are treated like functions

typealias SymOrSyms @compat(Union{Sym, Plots.AVec{Sym}})
Plots.convertToAnyVector(ex::Sym; kw...) = Any[ex], nothing
function Plots.computeY(xs, ex::Sym)
    u = free_symbols(ex)[1]
    mapsubs(ex, u, xs)
end


function mapSymOrSyms(f::Sym, xs::Plots.AVec)
    u = free_symbols(f)[1]
    mapsubs(f, u, xs) ## much faster than Float64[ex(x) for x in xs]
end
    
mapSymOrSyms(fs::Plots.AVec{Sym}, xs::Plots.AVec) = [mapSymOrSyms(f, xs) for f in fs]

## # contours or surfaces... 
function Plots.createKWargsList{T<:Real,S<:Real}(plt::Plots.PlottingObject, x::Plots.AVec{T}, y::Plots.AVec{S}, zf::Sym; kw...)
    # only allow sorted x/y for now
    # TODO: auto sort x/y/z properly
    @assert x == sort(x)
    @assert y == sort(y)

    fs=free_symbols(zf)
    surface = mapsubs2(zf, fs[1], x, fs[2], y)
    Plots.createKWargsList(plt, x, y, surface; kw...)  # passes it to the zmat version
end


# list of expressions
function Plots.createKWargsList(plt::Plots.PlottingObject, f::SymOrSyms, x; kw...)
    @assert !(typeof(x) <: Sym)  # otherwise we'd hit infinite recursion here
    Plots.createKWargsList(plt, x, f; kw...)
end

# special handling... xmin/xmax with function(s)
function Plots.createKWargsList(plt::Plots.PlottingObject, f::SymOrSyms, xmin::Real, xmax::Real; kw...)
    width = plt.plotargs[:size][1]
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

##################################################
## Helper functions

## the fallback pattern Float64[ex(x) for x in xs] has the
## disadvantage of several round trips between Julia and Python (and
## being v0.3 incompatible.  This functions tries to pass the xs all at once
## to Python, has it loop there, and then return the values. They
## come back as an array of type Any, so need conversion.
function mapsubs(ex::Sym, x::Sym, vals::AbstractVector)
    out = Float64[]
    try
        out = map(lambdify(ex), vals)
        out = map(Float64, out)
    catch err
        try
            out = pyeval("[fn(x,val) for val in vals]", fn=project(ex)[:subs], x=project(x), vals=vals)
            out = map(Float64, out)
        catch err
            out = Float64[ex(x) for x in vals]
        end
    end
    out
end

## This is similar to the above, but is used for two variables. It
## mimics Float64[ex(x,y) for x in xs, y in ys]
function mapsubs2(ex::Sym, x,xs, y, ys)
    out = Float64[]
    try
        out = map(lambdify(ex,[x,y]), xs, ys)
        out = map(Float64, out)
    catch err
        try
            out = PyCall.pyeval("[fn(x,xval).subs(y,yval) for yval in yvals for xval in xvals]",
                                fn=ex.x[:subs],x=x.x,y=y.x,xvals=xs, yvals=ys)
            out = reshape(out, (length(xs), length(ys)))
            out = map(Float64, out)
        catch err
            out =Float64[ex(x,y) for x in xs, y in ys]
        end
    end
    out
end



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
quiver, contour3D, and plot_implicit.
See ?sympy_plotting for some more details
""")

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

            us = mapsubs2(exs[1], U, xs, V,ys)
            vs = mapsubs2(exs[2], U, xs, V,ys)
            
            PyPlot.quiver(xs, ys, us, vs, args...; kwargs...)
        end
        ## deprecate this. We leave `quiver` for internal use.
        ##global  vectorplot = PyPlot.quiver
        
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

            zs = mapsubs2(ex, U, xs, V,ys)
            PyPlot.contour3D(xs, ys, zs, args...; kwargs...)
        end
        
        

        ## Implict equations plot
        ## XXX: We use the sympy interface here, as PyPlot does not have an interface for implicit plots
        ## XXX: There are others that *could* be used here:
        plot_implicit(ex, args...; kwargs...) = SymPy.call_sympy_fun(sympy[:plotting][:plot_implicit], ex, args...; kwargs...)


        eval(Expr(:export, :contour3D))
        eval(Expr(:export, :vectorplot))
        eval(Expr(:export, :quiver))
        eval(Expr(:export, :add_arrow))
        eval(Expr(:export, :plot_implicit))
        
    end

end
