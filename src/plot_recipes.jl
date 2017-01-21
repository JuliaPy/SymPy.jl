"""

Plotting of symbolic objects.

The `Plots` package provide a uniform interface to many of `Julia`'s
plotting packages. `SymPy` plugs into `Plots`' "recipes."

The basic goal is that when `Plots` provides an interface for function
objects, this package extends the interface to symbolic expressions.

In particular:


* `plot(ex::Sym, a, b; kwargs...)` will plot a function evaluating `ex` over [a,b]

Example. Here we use the default backend for `Plots` to make a plot:

```
using Plots
@vars x
plot(x^2 - 2x, 0, 4)
```


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

For a few backends (those that support `:path3d`) a third symbolic
expression may be added to have a 3d parametric plot rendered:

```
plot(sin(x), cos(x), x, 0, 4pi) # helix in 3d
```

* `plot(xs, ys, expression)` will make a contour plot (for many backends).

```
@vars x y
plot(linspace(0,5), linspace(0,5), x*y)
```



* To plot the surface  `z=ex(x,y)` over a region we have `Plots.surface`. For example,

```
@vars x y
surface(-5:5, -5:5, 25 - x^2 - y^2)
```

* a vectorfield plot can (inefficiently but directly) be produced following this example:

```
function vfieldplot(fx, fy; xlim=(-5,5), ylim=(-5,5), n=7)
    xs = linspace(xlim..., n)
    ys = linspace(ylim..., n)

    us = vec([x for x in xs, y in ys])
    vs = vec([y for x in xs, y in ys])
    fxs = vec([fx(x,y) for x in xs, y in ys])
    fys = vec([fy(x,y) for x in xs, y in ys])

    quiver(us, vs, quiver=(fxs, fys))
end
fx = (x + y) / sqrt(x^2 + y^2)
fy = (x - y) / sqrt(x^2 + y^2)
vfieldplot(fx, fy)
```


----

Some graphics provided by `SymPy` are available if `PyPlot` is installed.


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


* `plot_implicit(equation, (xvar, x0, x1), (yvar, y0, y1))` will plot implicitly the equation.

```
@syms x y
plot_implicit(Eq(x^2+ y^2,3), (x, -2, 2), (y, -2, 2))  # draw a circle
```


"""
sympy_plotting = nothing
export sympy_plotting


## Recipes for hooking into Plots

using RecipesBase

##
_lambdify(ex) = x -> N(ex(x))
@recipe f{T<:Sym}(::Type{T}, v::T) = _lambdify(v)

## for vectors of expressions
@recipe f{S<:AbstractVector{Sym}}(::Type{S}, ss::S) = Function[_lambdify(s) for s in ss]

## vectorfieldplot is an addon as we avoid quiver
## Not ready yet!
## @recipe function f(::Type{Val{:vectorfieldplot}}, fx, fy, xlim=(-5,5), ylim=(-5,5); n=15)

    
##     x₀, x₁ = xlim
##     y₀, y₁ = ylim
##     Δx = (x₁ - x₀) / (n-1)
##     Δy = (y₁ - y₀) / (n-1)

##     xs = x₀:Δx:x₁
##     ys = y₀:Δy:y₁

##     mx, my = 0.0, 0.0

##     ## two loops, first to identify scaling factor so
##     ## lines stay within box of width Δx by Δy
##     for x in xs, y in ys
##         mx = max(mx, abs(fx(x,y)))
##         my = max(my, abs(fy(x,y)))
##     end

##     ## we want all lines to be in the box, so we scale
##     λ = .95 *  min(Δx/mx, Δy/my)

##     XS = Float64[]
##     YS = Float64[]
    
##     for x in xs, y in ys
##         append!(XS, [x, x + λ * fx(x,y), NaN])
##         append!(YS, [y, y + λ * fy(x,y), NaN])
##     end
##     pop!(XS); pop!(YS)

    
##     seriestype := :path
##     xlims := xlim
##     ylims := ylim
##     legend := false


##     x := XS
##     y := YS

##     ()
## end

## function vectorfieldplot(fx::Sym, fy::Sym, xlim=(-5,5), ylim=(-5,5); n::Int=15, kwargs...)
##     xs = free_symbols([fx, fy])
##     vectorfieldplot(lambdify(fx, xs) , lambdify(fy, xs), xlim, ylim; n=n, kwargs...)
## end

## function vectorfieldplot!(fx::Sym, fy::Sym, xlim=(-5,5), ylim=(-5,5); n::Int=15, kwargs...)
##     xs = free_symbols([fx, fy])
##     vectorfieldplot!(lambdify(fx, xs), lambdify(fy, xs), xlim, ylim; n=n, kwargs...)
## end

## function vectorfieldplot(fx, fy, xlim=(-5,5), ylim=(-5,5); n::Int=15, kwargs...)
##     try
##         eval(Expr(:using, :Plots)) # XXX Super hacky way to do this
##     catch err
##         warn("Can't plot without the `Plots` package.")
##     end
##     plot(Val{:vectorfieldplot}, fx, fy, xlim, ylim; n=n, kwargs...)
## end

## function vectorfieldplot!(fx, fy, xlim=(-5,5), ylim=(-5,5); n::Int=15, kwargs...)
##     try
##         eval(Expr(:using, :Plots)) # XXX Super hacky way to do this
##     catch err
##         warn("Can't plot without the `Plots` package.")
##     end
##     plot!(Val{:vectorfieldplot}, fx, fy, xlim, ylim; n=n, kwargs...)
## end
## export vectorfieldplot, vectorfieldplot!

## ---------------------



## These functions give acces to SymPy's plotting module. They will work if PyPlot is installed, but may otherwise cause an error

## surface plot xvar = Tuple(Sym, Real, Real)
##
"""

Render a parametrically defined surface plot.

Example:
```
@vars u, v
plot_parametric_surface((u*v,u-v,u+v), (u,0,1), (v,0,1))
```

This uses `PyPlot`, not `Plots` for now.
"""
function plot_parametric_surface(exs::(@compat Tuple{Sym,Sym,Sym}),
                                 xvar=(-5.0, 5.0),
                                 yvar=(-5.0, 5.0),
                                 args...;
                                 kwargs...)

    SymPy.call_sympy_fun(sympy[:plotting][:plot3d_parametric_surface], exs..., args...; kwargs...)
    
end
export plot_parametric_surface





"""
Plot an implicit equation

```
@syms x y
plot_implicit(Eq(x^2+ y^2,3), (x, -2, 2), (y, -2, 2))
```

"""
plot_implicit(ex, args...; kwargs...) = SymPy.call_sympy_fun(sympy[:plotting][:plot_implicit], ex, args...; kwargs...)
export plot_implicit
