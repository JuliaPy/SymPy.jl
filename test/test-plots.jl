## code to test plotting
## not part of headless test suite
## has dependencies not in Test.REQUIRES

using SymPy
Plots.plotly()

@vars x y

info("expression")
plot(x^2, 0, 3)                   # single expression
plot!(9-x^2, 0, 3, color=:green)  # plot!

info("two graphs")
plot([x^2, 1 - x^2/2], 0, 3)      # two graphs at once

plot(x^2, 0, 3)
plot!(1 - x^2/2, 0, 3)


info("parametric plot")
plot(sin(x), cos(x), 0, 2pi)    # parametric
parametricplot(sin(x), cos(x), 0, 2pi)


info("3d parameteric")
parametricplot(cos(x), sin(x), x, 0, pi, linewidth=5)


info("vectorplot")
vectorfieldplot([cos(x), sin(y)], (x,0,2pi), (y,0,2pi))

info("contour plot")
plot(linspace(-5,5,25), linspace(-5,5,25), x^2 - y^2)
contourplot(x^2 - y^2, (x,-5, 5), (y,-5, 5))

info("surface")
plot_surface(x^2 + y^2, (x,-5, 5), (y,-5, 5))


## PyPlot Only for now.

info("parametric surface")
plot_parametric_surface((sin(x)*sin(y), sin(x)*cos(y), cos(x)), (x,0,pi/2), (y,0,pi))


info("implicit")
plot_implicit(x^2 + y^2 <= 1, (x,-5, 5), (y, 0,5))
