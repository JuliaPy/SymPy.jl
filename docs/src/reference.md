# Reference/API


```@index
Pages = ["reference.md"]
```

```@meta
DocTestSetup = quote
  using SymPy
end
```

## Inspection

```@docs
```


## Mathematical Functions

```@docs
```

## Plotting

Polynomials can be plotted directly using [Plots.jl](https://github.com/juliaplots/plots.jl).

```julia

```


```julia

```


### Example: The Polynomials.jl logo

```@example
using Plots, Polynomials
# T1, T2, T3, and T4:
chebs = [
  ChebyshevT([0, 1]),
  ChebyshevT([0, 0, 1]),
  ChebyshevT([0, 0, 0, 1]),
  ChebyshevT([0, 0, 0, 0, 1]),
]
colors = ["#4063D8", "#389826", "#CB3C33", "#9558B2"]
itr = zip(chebs, colors)
(cheb,col), state = iterate(itr)
p = plot(cheb, c=col,  lw=5, legend=false, label="")
for (cheb, col) in Base.Iterators.rest(itr, state)
  plot!(cheb, c=col, lw=5)
end
savefig("sympy.svg"); nothing # hide
```

![](sympy.svg)
