
import PyCall
PyCall.pyimport_conda("sympy.physics.wigner",       "sympy")
PyCall.pyimport_conda("sympy.physics.optics",       "sympy")
PyCall.pyimport_conda("sympy.physics.quantum.spin", "sympy")

## to use
## import_from(sympy.physics.wigner)
## wigner3j(sympy.S(6),0,4,0,2,0)
## Wigner3j(sympy.S(6),0,4,0,2,0).doit()
