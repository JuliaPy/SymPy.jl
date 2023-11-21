using BenchmarkTools
using SymPy

const SUITE = BenchmarkGroup()

SUITE["creation"] = BenchmarkGroup(["sympy", "creation"])
SUITE["expand"] = BenchmarkGroup(["sympy", "expand"])

SUITE["creation"]["symbols","single"] = @benchmarkable symbols("x")
SUITE["creation"]["symbols","multiple"] = @benchmarkable symbols("x,y,z")
SUITE["creation"]["symbols","assumptions"] = @benchmarkable symbols("x", real=true)
SUITE["creation"]["Sym","single"] = @benchmarkable Sym("x")
SUITE["creation"]["Sym","multiple"] = @benchmarkable Sym("x,y,z")

SUITE["creation"]["@syms","single"] = @benchmarkable @syms
SUITE["creation"]["@syms","multiple"] = @benchmarkable @syms x, y, z
SUITE["creation"]["@syms","assumptions"] = @benchmarkable @syms x::real

SUITE["creation"]["Sym","number"] = @benchmarkable Sym(2)
SUITE["creation"]["Sym","boolean"] = @benchmarkable Sym(true)


SUITE["expand"]["expression","call"] = @benchmarkable sin(x) setup=(x=symbols("x"))
SUITE["expand"]["expression","sympy call"] = @benchmarkable sympy.expand_trig(sin(2x)) setup=(x=symbols("x"))
SUITE["expand"]["expression","promotion"] = @benchmarkable x^2 - x + inv(x) - 1/x^2 setup=(x=symbols("x"))

SUITE["expand"]["n",5] = @benchmarkable expand((x-a)^5) setup=(x=symbols("x");a=symbols("a"))
SUITE["expand"]["n",50] = @benchmarkable expand((x-a)^50) setup=(x=symbols("x");a=symbols("a"))
