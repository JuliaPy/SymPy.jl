## in mpmath module
## These are not right!!!
## see hyper and meijerg to indicate what needs to be done for these special function
## they really need to be coordinated with `Julia`'s as well.
mpmath_fns = (:hyp0f1, 
           :hyp1f1, :hyp1f2, 
           :hyp2f0, :hyp2f1, :hyp2f2, :hyp2f3,
           :hyp3f2,
           :hypercomb,
#           :meijerg,
           :bihyper,
           :hyper2d,
           :appellf1, :appellf2, :appellf3, :appellf4,
           :ber,:bei,:ker,:kei,
           :struveh,:struvel,
           :angerj,
           :webere,
           :coulombc,
           :legenp, :legenq,
           :chebyt, :chebyu, 
           :pcfd, :pcfu, :pcfv, :pcfw,
           :lommels1, :lommels2,
           :coulombf, :coulombg,
           :hyperu,
           :whitm, :whitw,
           :scorergi, :scorerhi,
           :spherharm,
           :airyaizero, :airybizero, 
           :besseljzero, :besselyzero
           )
for fn in mpmath_fns
    meth = string(fn)
    #    @eval ($fn)(xs::SymOrNumber...;kwargs...) = mpmath_meth(@compat(Symbol($meth)), xs...; kwargs...)
    @eval ($fn)(xs::SymOrNumber...;kwargs...) = mpmath_meth($meth, xs...; kwargs...)
    eval(Expr(:export, fn))
end

## Initialize mpmath
## includes trying to find the module!
## automatic mappings may throw warning about strings, though it is expected these
## will be addressed by PyCall
function init_mpmath()
    try
        PyCall.mpmath_init()
    catch err
        return()
    end
    ## try to load mpmath module
    try
        copy!(mpmath, pyimport("sympy.mpmath"))
    catch e
        try
	    copy!(mpmath, pyimport("mpmath"))			
        catch e
            if PyCall.conda
                info("Installing mpmath via the Conda package...")
                #Conda.add("mpmath")
                PyCall.pyimport_conta("mpmath", "mpmath")
                copy!(mpmath, pyimport("mpmath"))
            else
                error("""Failed to pyimport("mpmath"): SymPy will have less functionality.

                      For automated mpmath installation, try configuring PyCall to use the Conda Python distribution within Julia.  Relaunch Julia and run:
                            ENV["PYTHON"]=""
                            Pkg.build("PyCall")
                            using SymPy

                      pyimport exception was: """, e)
            end
        end
    end
    if mpmath != PyCall.PyNULL()
        
        
        ## ignore warnings for now...
        mpftype = mpmath["mpf"]
        pytype_mapping(mpftype, BigFloat) ## Raises warning!!!
        mpctype = mpmath["mpc"]
        pytype_mapping(mpctype, Complex{BigFloat})
    end

    ## Call a function in the mpmath module, giving warning and returning NaN if module is not found 
    global mpmath_meth(meth, args...; kwargs...) = begin
        if isa(mpmath, Void)
            warn("The mpmath module of Python is not installed. http://docs.sympy.org/dev/modules/mpmath/setup.html#download-and-installation")
            return(Sym(NaN))
        end

        fn = mpmath[@compat(Symbol(meth))]
        ans = call_sympy_fun(fn, args...; kwargs...)
        ## make nicer...
        if isa(ans, Vector)
            ans = Sym[i for i in ans]
        end
        ans
    end
end
