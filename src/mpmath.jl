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
    @eval ($fn)(xs::SymOrNumber...;kwargs...) = mpmath_meth($meth, xs...; kwargs...)
    eval(Expr(:export, fn))
end

## Call a function in the mpmath module, giving warning and returning NaN if module is not found
## (Doesn't need to be in init_mpmath?)
function mpmath_meth(meth, args...; kwargs...) 
    if isa(mpmath, Nothing)
        warn("The mpmath module of Python is not installed. http://docs.sympy.org/dev/modules/mpmath/setup.html#download-and-installation")
        return(Sym(NaN))
    end
    
    fn = mpmath[Symbol(meth)]
    ans = call_sympy_fun(fn, args...; kwargs...)
    ## make nicer...
    if isa(ans, Vector)
        ans = convert(Vector{Sym}, ans)
    end
    ans
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
    copy!(mpmath, PyCall.pyimport_conda("mpmath", "mpmath"))
    if mpmath != PyCall.PyNULL()
        ## ignore warnings for now...
        mpftype = mpmath["mpf"]
        pytype_mapping(mpftype, BigFloat) ## Raises warning!!!
        mpctype = mpmath["mpc"]
        pytype_mapping(mpctype, Complex{BigFloat})
    end

    
end

