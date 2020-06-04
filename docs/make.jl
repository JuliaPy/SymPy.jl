using Documenter
using SymPy

makedocs(
sitename = "SymPy",
format = Documenter.HTML(),
modules = [SymPy],
pages = [
    "Home" => "index.md",
    "Examples"  =>  "introduction.md",
    "SymPy tutorial"=> [
        "About" =>  "Tutorial/index.md",
        "Introduction" => "Tutorial/intro.md",
        "Gotchas" => "Tutorial/gotchas.md",
        "Basic operations" => "Tutorial/basic_operations.md",
        "Simplification" => "Tutorial/simplification.md",
        "Calculus" => "Tutorial/calculus.md",
        "Solvers" => "Tutorial/solvers.md",
        "Matrices" => "Tutorial/matrices.md",
        "Advanced expression  manipulation" => "Tutorial/manipulation.md"
    ],      
    "Reference/API" => "reference.md"
    ],
)

# Documenter can also automatically deploy documentation to gh-pages.
# See "Hosting Documentation" and deploydocs() in the Documenter manual
# for more information.
#=deploydocs(
    repo = "<repository url>"
)=#
