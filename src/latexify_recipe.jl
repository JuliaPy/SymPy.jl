
using Latexify: Latexify

# Recipe to hook into Latexify.jl's `latexify` function for `Sym`s.
# We do not use SymPy's `latex` function here since it behaves a bit different
# from Latexify.jl. Thus, we let Latexify.jl handle everything for the sake of
# consistency.
# For example, SymPy doesn't print a multiplication symbol by default while
# Latexify uses a `"\\cdot"` (unless the keyword argument `cdot=false` is set).
Latexify.@latexrecipe function _(x::Sym)
  return string(x)
end
