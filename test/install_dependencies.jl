# copied and modified from https://github.com/tkf/IPython.jl/blob/master/test/install_dependencies.jl


# Adding Pkg in test/REQUIRE would be an error in 0.6.  Using
# Project.toml still has some gotchas.  So:
Pkg = Base.require(Base.PkgId(Base.UUID(0x44cfe95a1eb252eab672e2afdf69b78f), "Pkg"))

# Let PyCall.jl use Python interpreter from Conda.jl
# See: https://github.com/JuliaPy/PyCall.jl
ENV["PYTHON"] = ""
Pkg.build("PyCall")

using SymPy
