Wigner  = PyCall.PyNULL()
Optics  = PyCall.PyNULL()
Spin = PyCall.PyNULL()

function init_physics()
    copy!(Wigner,  PyCall.pyimport_conda("sympy.physics.wigner",       "sympy"))
    copy!(Optics,  PyCall.pyimport_conda("sympy.physics.optics",       "sympy"))
    copy!(Spin ,   PyCall.pyimport_conda("sympy.physics.quantum.spin", "sympy"))
end


module Physics

using PyCall
using SymPy

physics = [(SymPy.Wigner, [:clebsch_gordan,
                           :dot_rot_grad_Ynm,
                           :gaunt,
                           :racah,
                           :wigner_3j,
                           :wigner_6j,
                           :wigner_9j]),
           (SymPy.Optics, [:RayTransferMatrix,
                           :FreeSpace,
                           :CurvedRefraction,
                           :FlatMirror]),
           (SymPy.Spin  , [:WignerD])
]


for (m, meths) in physics
    for meth in meths
        meth_name = string(meth)
        @eval begin
            # @doc """
            # `$($meth_name)`: a SymPy function.
            #     The SymPy documentation can be found through: http://docs.sympy.org/latest/search.html?q=$($meth_name)
            #     """ ->
            ($meth)(args...;kwargs...) = pycall(getproperty($m, $meth_name), PyAny, map(Sym, args)...,
                                                [k=>Sym(v) for (k,v) in pairs(kwargs)]...)
        end
        eval(Expr(:export, meth))
    end
end

end
