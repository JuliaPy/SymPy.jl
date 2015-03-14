

## Imported math functions
## make vectorized version while we are at it
for fn in (:sin, :cos, :tan, :sinh, :cosh, :tanh, :asin, :acos, :atan,
           :asinh, :acosh, :atanh, :sec, :csc, :cot, :asec, :acsc, :acot, 
           :sech, :csch, 
           :coth, :acoth, 
           :atan2,
           :radians2degrees, :degrees2radians,
           :log2, :log10, :log1p, :exponent, :exp, :exp2, :expm1,
           :sqrt, :square, :erf, :erfc, :erfcx, :erfi, :dawson,
           :ceiling, :floor, :trunc, :round, :significand,
           :factorial2,
           :airyai, :airybi
           )
    meth = string(fn)
    @eval ($fn)(x::Sym;kwargs...) = getfield(sympy,symbol($meth))(project(x),[(k,project(v)) for (k,v) in kwargs]...)
    @eval ($fn)(a::Array{Sym}) = map($fn, a)
end

## Handle arguments differently
log(x::Sym) = sympy.log(project(x))
log(b::Sym, x::Sym) = sympy.log(project(x), project(b))

## :asech, :acsch, :sinc, :cosc, 
## These fail, so define from definitions
## http://mathworld.wolfram.com/InverseHyperbolicSecant.html
asech(z::Sym) = log(sqrt(1/z-1)*sqrt(1/z+1) + 1/z)
asech(as::Array{Sym}) = map(asech, as)
## http://mathworld.wolfram.com/InverseHyperbolicCosecant.html
acsch(z::Sym) = log(sqrt(1+1/z^2) + 1/z)
acsch(as::Array{Sym}) = map(acsch, as)
sinc(x::Sym) = sin(Sym(PI*x))/(PI*x)
sinc(as::Array{Sym}) = map(sinc, as)
cosc(x::Sym) = diff(sinc(x))
cosc(as::Array{Sym}) = map(cosc, as)



## these have (parameter, x) signature. Use symbolic x to call sympy version, othewise
## should dispatch to julia version.
for fn in (:besselj, :bessely, :besseli, :besselk)
    meth = string(fn)
    @eval ($fn)(nu::Union(Sym, Number), x::Sym;kwargs...) = getfield(sympy,symbol($meth))(project(nu), project(x),[(k,project(v)) for (k,v) in kwargs]...)
    @eval ($fn)(nu::Union(Sym, Number), a::Array{Sym}) = map(x ->$fn(nu, x), a)
end

## export these sympy functions ...

## (x:Sym, ...) , export
sympy_math_methods = (:Prod,
                      :Ylm, 
                      :assoc_legendre, 
                      :chebyshevt
                      )
for meth in sympy_math_methods 
    meth_name = string(meth)
    @eval ($meth)(ex::Sym, args...) = Sym(getfield(sympy,symbol($meth_name))(project(ex), project(args)...))
    eval(Expr(:export, meth))
end

## :gamma, :beta, # need import
beta(x::Sym, y::Sym) = sympy.beta(project(x), project(y))
gamma(x::Sym, y::Sym) = sympy.gamma(project(x), project(y))


## simple (x::Union(Sym, Number;...) signature, export
for fn in (
           :hankel1, :hankel2,             # hankel function of second kind H_n^2(x) = J_n(x) - iY_n(x)
           :legendre,
           :jacobi, 
           :gegenbauer,
           :hermite,
           :laguerre
           )
    meth = string(fn)
    @eval ($fn)(xs::Union(Sym, Number)...;kwargs...) = 
    getfield(sympy,symbol($meth))([project(x) for x in xs]...,[(k,project(v)) for (k,v) in kwargs]...)
    eval(Expr(:export, fn))
end

## in mpmath module

for fn in (:hyp0f1, 
           :hyp1f1, :hyp1f2, 
           :hyp2f0, :hyp2f1, :hyp2f2, :hyp2f3,
           :hyp3f2,
           :hyper, :hypercomb,
           :meijerg,
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
           meth = string(fn)
           @eval ($fn)(xs::Union(Sym, Number)...;kwargs...) = 
           Sym(mpmath.((symbol($meth)))([project(x) for x in xs]...,[(k,project(v)) for (k,v) in kwargs]...))
    eval(Expr(:export, fn))
end
           


## in julia, not SymPy
cbrt(x::Sym) = PyCall.pyeval("x ** (1/3)", x=project(x)) 
Base.ceil(x::Sym) = ceiling(x)

## degree functions   
for fn in (:cosd, :cotd, :cscd, :secd, :sind, :tand,
          :acosd, :acotd, :acscd, :asecd, :asind, :atand)

    rad_fn = string(fn)[1:end-1]
    @eval ($fn)(x::Sym) = getfield(sympy, symbol($rad_fn))(project(x * Sym(sympy.pi)/180))
    @eval ($fn)(a::Array{Sym}) = map($fn, a)
end
                                           


## add
abs(x::Sym) = sympy_meth(:Abs, x)
abs(a::Array{Sym}) = map(abs, a)

Base.isless(a::Real, b::Sym) = isless(a, convert(Float64, b))
Base.isless(a::Sym, b::Real) = isless(b, a)
Base.isfinite(x::Sym) = isfinite(convert(Float64, x))

## Some sympy function interfaces

## subs
"""

`subs` is used to subsitute a value in an expression with another value. The `replace` function is also overrided to do this task.


""" 
function subs{T <: SymbolicObject, S <: SymbolicObject}(ex::T, x::S, arg)
    object_meth(ex, :subs, x, convert(Sym,arg))
end
subs{T <: SymbolicObject, S <: SymbolicObject}(exs::Array{T}, x::S, arg) = map(ex->subs(ex, x, arg), exs)

## curried version to use with |>
subs(x::SymbolicObject, y) = ex -> subs(ex, x, y)

"""

Substitute multiple values at once with `subs(ex, (var1, val1), (var2, val2), ...)`

"""
function subs(ex::SymbolicObject, x::(SymbolicObject, Any), args...)
    ex = subs(ex, x[1], x[2])
    if length(args) > 0
        subs(ex, args...)
    else
        ex
    end
end

"""

substitute multiple values stored in array

"""
subs{T <: Any}(ex::SymbolicObject, xs::Vector{(Sym,T)}) = subs(ex, xs...)

## convenience method to use symbol
subs{T <:SymbolicObject}(ex::T, x::Symbol, arg) = subs(ex, Sym(x), arg)

Base.replace(ex::SymbolicObject, x::SymbolicObject, y) = subs(ex, x, y)
## curried version to use through |> as in
## ex |> replace(x, 2)
Base.replace(x::SymbolicObject, y) = ex -> subs(ex, x, y)




function !={T <: Real}(x::Sym, y::T) 
    try 
        x = convert(Float64, x)
        x != y
    catch
        true
    end
end
function !={T <: Complex}(x::Sym, y::T) 
    try 
        x = complex(x)
        x != y
    catch
        true
    end
end

## helper, as :is_rational will find 1.2 rational...
_is_rational(ex::Sym) = ex[:is_rational] && ex[:numer]()[:is_integer]

## evalf, n, N
## We want to convert to numeric:
## evalf -> keep as sympy expression
## N bring back into julia using convert
## In SymPy N and evalf are the same, so this could be confusing!!!

"""

Convert a `Sym` value to a numeric Julian value.

The `N` function of SymPy is an alias for `evalf`. Within SymPy, either may be used to
find numeric values from symbolic values.

For example, symbolic roots can be computed numerically, even if not
available symbolically, by calling `N` on the values.

Using `SymPy` within `Julia` makes having two such functions useful:

* one to do the equivalent of SymPy's `evalf` function 
* one to *also* convert these expressions back into `Julia` objects.

We use `N` to return a `Julia` object and `evalf` to return a symbolic
object.

Examples:
```
x = Sym("x")
p = subs(x, x, pi)
N(p)                            # float version of pi
evalf(p, 60)                    # 60 digits of pi, as a symbolic value
N(p, 60)                        # when a precision is given, "Big" values are returned
r = subs(x,x,1.2)
N(r)                            # float
q = subs(x, x, 1//2)
N(q)                            # 1//2
z = solve(x^2 + 1)[1]           # -ⅈ
N(z)                            # 0 - 1im
evalf(z)

rts = solve(x^5 - x + 1)
[N(r) for r in rts]             # numeric solutions to quintic
```


The `evalf` function is similar, though it leaves the expression as a symbolic object.
This breaks the similarity of N and evalf for sympy users.

Throws a `DomainError` if no conversion is possible, such as when the expression still has symbolic values.

`N` is type unstable.

"""
function N(ex::Sym)
    ## more work than I'd like
    if ex[:is_integer] == nothing
        return(N(ex[:evalf]()))
    end
    if ex[:is_integer]
        for T in [Int, BigInt]
            try (return(convert(T, ex))) catch e end
        end
    elseif _is_rational(ex)
        try (return(convert(Rational, ex))) catch e end
    elseif ex[:is_real]
        for T in [MathConst, Float64] ## BigFloat???
              try (return(convert(T, ex))) catch e end
        end
    elseif ex[:is_complex]
        try
            r, i = ex[:re](), ex[:im]()
            r, i = promote(N(r), N(i))
            return(Complex(r, i))
        catch e
        end
    end
    throw(DomainError())
end

"""
`N` can take a precision argument. 

When given as an integer greater than 16, we try to match the digits of accuracy using `BigFloat` precision on conversions to floating point.

"""
function N(x::Sym, prec::Int)
    ## check
    prec <= 16 && return(N(x))
    if x[:is_integer] == nothing
        out = x[:evalf](prec)
        return( N(out, prec) )
    end
    
    ex = evalf(x, prec)
    if x[:is_integer]
        return(convert(BigInt, ex))
    elseif _is_rational(x)
        return(convert(Rational, ex))
    elseif x[:is_real]
        p = round(Int,log2(10)*prec)
        
        out = with_bigfloat_precision(p) do 
            convert(BigFloat, ex)
        end
        return(out)
    elseif x[:is_complex]
        r, i = ex[:re](), ex[:im]()
        u, v = promote(N(r, prec), N(i, prec))
        return(Complex(u, v))
    end
    
    throw(DomainError())
end

"""

The `evalf` function has keyword possibilities, such as `subs` being given directly through a `Dict(Sym,Any)`.
Unlike `N`, `evalf` returns an object of type `Sym`.

Examples
```
x = Sym("x")
evalf(x, subs=[x => 1/2])  # v0.3 syntax for anonymous dicts
```
"""
evalf(x::Sym, args...; kwargs...) = x[:evalf](args...; kwargs...)

for meth in (:separate, :flatten, 
             :igcd, :ilcm,
             :sqf,
             :together, 
             :limit, 
             :diff, :Derivative
             )
    meth_name = string(meth)
    @eval ($meth)(ex::Sym, args...; kwargs...) = sympy_meth(symbol($meth_name), ex, args...; kwargs...)
    eval(Expr(:export, meth))
end



## diff for matrix doesn't handle vectors well, so we vectorize here
diff(exs::Array{Sym}, args...; kwargs...) = map(ex -> diff(ex, args...;kwargs...), exs)

## bring over for function calls (not expressions)
## returns a symbolic expression
## limit(f, c) or limit(f,c,dir="-") or limit(f, c, dir="-")
function limit(f::Function, c::Number=0; kwargs...)
    x = Sym("x")
    ## catch some values...
    if abs(c) == Inf
        c = sign(c) * oo
    elseif c == Base.pi
        c = Sym(sympy.pi)
    elseif c == Base.e
        c = Sym(sympy.exp(1))
    end
    limit(f(x), x, c; kwargs...)
end

## find symbolic derivatives from a function
function diff(f::Function, k::Int=1)
    x = Sym("x")
    diff(f(x), x, k)
end
    


## different conversions
fraction(args...; kwargs...) = sympy.fraction(project(args)...; kwargs...) |> os -> map(u -> convert(Sym, u), os)





## Special functions
## Spherical harmonic




## solve

## DEPRECATED
## Is this a good idea? I want to be able to solve equations with
## solve(x^2 +x == x, x)
##==(x::Sym, y::Sym) = solve(x - y)

## Handle ininf, and isnan by coercion to float
Base.isinf(x::Sym) = try isinf(convert(Float64, x)) catch e false end
Base.isnan(x::Sym) = try isnan(convert(Float64, x)) catch e false end


Base.div(x::Sym, y::Union(Sym, Number)) = convert(Sym, sympy.floor(project(x/convert(Sym,y))))

Base.rem(x::Sym, y::Union(Sym, Number)) = x-Sym(y)*Sym(sympy.floor(project(x/y)))

Base.zero(x::Sym) = Sym(0)
Base.zero(::Type{Sym}) = Sym(0)

Base.one(x::Sym) = Sym(1)
Base.one(::Type{Sym}) = Sym(1)

"""

Solve an expression for any zeros or a system of expressions passed a vector.

Examples: 

```
x,y, a,b,c = symbols("x, y, a, b, c", real=true)
solve(x^2 - x + 1)
solve(a*x^2  + b*x + c, x)
solve([x - y - 1, x + y - 2], [x,y])
solve([x - y - 1, x + y - 2])   # same as above, as it will assume all free variables.
```

Can solve a single expression for a lone free variable, or if a variable is passed as a second argument, solve for that variable in terms of the others.

When passed a vector of expressions, `solve` looks for solutions to
the system of m equations with n unknowns, where the equations and
unknowns are passed as `Vector{Sym}`. If the unknowns are not
specified, all the free symbols will be used.


The `SymPy` docs say this about `solve`:

> Note If solve returns [] or raises NotImplementedError, it
> doesn’t mean that the equation has no solutions. It just means
> that it couldn’t find any. Often this means that the solutions
> cannot be represented symbolically. For example, the equation
> `x=cos(x)` has a solution, but it cannot be represented
> symbolically using standard functions.
> 
> In fact, solve makes no guarantees whatsoever about the completeness
> of the solutions it finds. Much of solve is heuristics, which may find
> some solutions to an equation or system of equations, but not all of
> them.
        

If succesful, returns an array of possible answers, a dictionary, or an array of dictionaries. The dictionaries are
of the form `string => Sym`, so to access the values, use `d[string(x)]` or `d["x"]`, but not `d[x]`, where `x` is symbolic.

Note: The individual components of the array display more nicely than the array.

Reference: [SymPy Docs](http://docs.sympy.org/0.7.5/modules/solvers/solvers.html#algebraic-equations)

"""  
function solve(ex::Sym, args...; kwargs...)
    a = sympy.solve(project(ex), map(project, args)...; kwargs...)

    ## Way too much work here to finesse into a nice enough output
    ## (Issue comes from solving without specifying variable when 2 or more variables in the expression
    if (length(a) > 0 && isa(a[1], Dict))
        d = Dict()
        for kv in a
            for (k,v) in kv
                d[k] = v
            end
        end
        d

        # d = Dict()
        # for kv in a
        #     for (k,v) in kv
        #         cur = collect(keys(d))
        #         i = findfirst(cur, k)
        #         if i > 0
        #             push!(d[cur[i]], v)
        #         else
        #             d[k] = [v]
        #         end
        #     end
        # end
        # for (k,v) in d
        #     if length(v) == 1
        #         d[k] = v[1]
        #     end
        # end
        # if length(d) == 1
        #     collect(values(d))[1]
        # else
        #     d
        # end
    else
        Sym[v for v in a]
    end
end


function solve(exs::Vector{Sym}, args...; kwargs...)
    ans = sympy.solve(map(project, exs),  args...; kwargs...) #  dictionary with keys, values as PyObjects
    tmp = map(get_free_symbols, exs)
    xs = shift!(tmp)
    for ss in tmp
        for s in ss
            if !(s in xs)
                push!(xs, s)
            end
        end
    end

    solve(exs, xs, args...; kwargs...)

end
function solve(exs::Vector{Sym}, xs::Vector{Sym}, args...; kwargs...)
    ans = sympy.solve(map(project, exs), map(project, xs), args...; kwargs...) #  dictionary with keys, values as PyObjects

    function mapit(out) ## can be a tuple if m=n
        d = Dict{String, Sym}()
        [d[string(xs[i])] = out[i] for i in 1:length(out)]
        d
    end
    function mapit(out::Dict)
        d = Dict{String,Sym}()
        [d[string(k)]=v for (k,v) in out]
        d
    end
    if isa(ans, Dict)
        mapit(ans)              # XXX type unstable! should be array...
    else
        map(mapit, ans)
    end
end


## Numeric solutions

"""
Numerically solve for a zero of an expression.

Examples: 
```
solve(x^5 - x -1) # inconclusive
nsolve(x^5 - x - 1, 1)
```

Reference: [SymPy Docs](http://docs.sympy.org/0.7.5/modules/solvers/solvers.html#algebraic-equations)
"""              
nsolve(ex::Sym, x::Sym, x0::Number) = sympy.nsolve(project(ex), project(x), x0) |> x -> convert(Float64, x)
nsolve(ex::Sym, x0::Number) =  sympy.nsolve(project(ex), x0) |> x -> convert(Float64, x)
function nsolve{T <: Number}(ex::Vector{Sym}, x::Vector{Sym}, x0::Vector{T}; kwargs...)
    ans = sympy.nsolve(tuple(map(project,ex)...), tuple(map(project,x)...), tuple(x0...); kwargs...)
    ## ans is matrix object -- convert
    convert(Array{Sym}, sympy.Matrix(ans)) |> x -> convert(Float64, x)
end
export nsolve

## dsolve
## Make a function argument, but munge arguments from Sym -> PyObject class
SymFunction(nm::Union(Symbol, String)) = (args...) -> Sym(sympy.Function(nm)(project(args)...))


"""

Solve an odinary differential equation.

Examples:

```
f = SymFunction("f")
x = Sym("x")
dsolve(diff(f(x), x) + f(x), f(x)) ## solve f'(x) + f(x) = 0
dsolve(diff(f(x), x, x) + f(x), f(x)) ## solve f''(x) + f(x) = 0
```

References: [SymPy Docs](http://docs.sympy.org/0.7.5/modules/solvers/ode.html#ode-docs)
"""             
dsolve(ex::Sym, fx::Sym) = sympy_meth(:dsolve, ex, fx)


"""

Create a piecewise defined function.

To create conditions on the variable, the functions `Lt`, `Le`, `Eq`, `Ge`, and `Gt` can be used. For infix notation, 
unicode operators can be used: `\ll<tab>`, `\le<tab>`, `\Equal<tab>`, `\ge<tab>`, and `\gg<tab>`.

To combine terms, the unicode `\vee<tab>` (for "or"), `\wedge<tab>` (for "and") can be used


Examples:
```
p = piecewise((1, x ≪ 1), (2, (1 ≤ x) ∨ (x ≤ 2)), (3, x ≫ 2)) ## using ∨ and ∧ for & and or
subs(p, x, 2) ## 2
x,a = symbols("x,a")
p = piecewise((1, Lt(x, a)), (2, Ge(x,a)))  # same as piecewise((1,  x ≪ a), (2, x ≥ a))
subs(p, x, a - 1)
```
"""                 
function piecewise(args...)
    args = [map(project, x) for x in args]
    sympy.Piecewise(args...)
end

## special numbers
"PI is a symbolic  π. Using `julia`'s `pi` will give round off errors." 
const PI = Sym(sympy.pi)

"E is a symbolic  `e`. Using `julia`'s `e` will give round off errors." 
const E = Sym(sympy.exp(1))

"IM is a symbolic `im`" 
const IM = Sym(sympy.I)

"oo is a symbolic infinity. Example: `integrate(exp(-x), x, 0, oo)`." 
const oo = Sym(sympy.oo)


## math constants
convert(::Type{Sym}, x::MathConst{:π}) = PI
convert(::Type{Sym}, x::MathConst{:e}) = E
convert(::Type{Sym}, x::MathConst{:γ}) = Sym(sympy.EulerGamma)
convert(::Type{Sym}, x::MathConst{:catalan}) =Sym(sympy.Catalan)
convert(::Type{Sym}, x::MathConst{:φ}) = (1 + Sym(5)^(1//2))/2
