using Documenter
using SymPy

makedocs(
sitename = "SymPy",
format = Documenter.HTML(),
modules = [SymPy],
pages = [
    "Home" => "index.md",
)

# Documenter can also automatically deploy documentation to gh-pages.
# See "Hosting Documentation" and deploydocs() in the Documenter manual
# for more information.
deploydocs(
    repo = "github.com/JuliaPy/SymPy.jl/"
)
