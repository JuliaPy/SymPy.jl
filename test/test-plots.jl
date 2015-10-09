## code to test plotting
## not part of headless test suite
## has dependencies not in Test.REQUIRES

XXX LOTS OF ISSUES with PyPlots XXX

using SymPy
using Plots
#backend(:unicodeplots)
plotter!(:unicodeplots)

@vars x

info("expression")
plot(x^2, 0, 3)                   # single expression

info("two graphs")
plot([x^2, 1 - x^2/2], 0, 3)      # two graphs

info("parametric plot")
plot((sin(x), cos(x)), 0, 2pi)    # parametric



## now clear out and use pyplot features
workspace()
using SymPy
using PyPlot
@vars x y

info("expression plot")
plot(x, 0,1)

info("expressions plot")
plot([x,x^2], 0,1)

info("parametric")
plot((cos(x), sin(x)), 0, 2pi)
parametricplot(cos(x), sin(x), 0, pi)

info("vectorplot")
vectorplot([cos(x), sin(y)], (x,0,2pi), (y,0,2pi))

info("contour plot")
contour(x^2 - y^2, (x,-5, 5), (y,-5, 5))

info("surface")
plot_surface(x^2 + y^2, (x,-5, 5), (y,-5, 5))

info("parametric surface")
plot_parametric_surface((sin(x)*sin(y), sin(x)*cos(y), cos(x)), (x,0,pi/2), (y,0,pi))


info("implicit")
plot_implicit(x^2 + y^2 <= 1, (x,-5, 5), (y, 0,5))
