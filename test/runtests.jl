if lowercase(get(ENV, "CI", "false")) == "true"
    include("install_dependencies.jl")
end


using SymPy

path = joinpath(pathof(SymPy.SymPyCore), "../../test")
include(joinpath(path, "runtests-sympycore.jl"))
