

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
                      :gamma, :beta, # need import
                      :assoc_legendre, 
                      :chebyshevt
                      )
for meth in sympy_math_methods 
    meth_name = string(meth)
    @eval ($meth)(ex::Sym, args...) = Sym(getfield(sympy,symbol($meth_name))(project(ex), project(args)...))
    eval(Expr(:export, meth))
end




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
           Sym(sympy.mpmath[(symbol($meth))]([project(x) for x in xs]...,[(k,project(v)) for (k,v) in kwargs]...))
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

Base.isless(a::Real, b::Sym) = isless(a, float(b))
Base.isless(a::Sym, b::Real) = isless(b, a)
Base.isfinite(x::Sym) = isfinite(float(x))

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
        x = float(x)
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

## evalf, n, N

for meth in (:n, :N,
             :separate, :flatten, 
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

## Logical operators for (Sym,Sym)
## Experimental! Not sure these are such a good idea ...
## but used with piecewise
Base.&(x::Sym, y::Sym) = PyCall.pyeval("x & y", x=project(x), y=project(y))
Base.|(x::Sym, y::Sym) = PyCall.pyeval("x | y", x=project(x), y=project(y))
!(x::Sym)         =      PyCall.pyeval("~x",    x=project(x))

## use ∨, ∧, ¬ for |,&,! (\vee<tab>, \wedge<tab>, \neg<tab>)
∨(x::Sym, y::Sym) = x | y
∧(x::Sym, y::Sym) = x & y
¬(x::Sym) = !x


"""

In SymPy, symbolic equations are not represented by `=` or `==`,
rather ther function `Eq` is used. Here we use the unicode
`\Equal<tab>` for an infix operator. There are also unicode values to represent `<`, `<=`, `>=`. `>`.

"""

## These are useful with plot_implicit, but they cause  issues elsewhere with linear algebra functions
#Base.isless(a::Sym, b::Sym) = Lt(a,b)
#Base.isequal(a::Sym, b::Sym) = Eq(a,b)
# ⩵(a::Sym, b::Sym) = isequal(a,b)
#export ⩵
## Instead we have:
## We use unicode for visual appeal of infix operators, but the Lt, Le, Eq, Ge, Gt are the proper way:

@doc "This is `\ll<tab>` mapped as an infix operator to `Lt`" ->
(≪)(a::Sym, b::Sym) = Lt(a,b)  # \ll<tab>
Base.(:≤)(a::Sym, b::Sym) = Le(a,b)  # \le<tab>

@doc "For infix `Eq` one can use \Equal<tab> unicode operator" ->
(⩵)(a::Sym, b::Sym) = Eq(a,b)  # \Equal<tab>

@doc "use equality if a python level." ->
==(x::Sym, y::Sym) = x.x == y.x

Base.(:>=)(a::Sym, b::Sym) = Ge(a,b)
(≫)(a::Sym, b::Sym) = Gt(a,b)



export ≪,⩵,≫


<(x::Sym,  y::Number) = PyCall.pyeval("x < y", x=project(x), y=project(y))
<=(x::Sym, y::Number) = PyCall.pyeval("x <= y", x=project(x), y=project(y))
>=(x::Sym, y::Number) = PyCall.pyeval("x >= y", x=project(x), y=project(y))
>(x::Sym, y::Number)  = PyCall.pyeval("x > y", x=project(x), y=project(y))
## hacky, but == is something else to SymPy
==(x::Sym, y::Number) = (x <= y) & (x >= y)

<(x::Number, y::Sym)  = y > x
<=(x::Number, y::Sym) = y >= x
>=(x::Number, y::Sym) = y <= x
>(x::Number, y::Sym)  = y < x


## Handle ininf, and isnan by coercion to float
Base.isinf(x::Sym) = try isinf(float(x)) catch e false end
Base.isnan(x::Sym) = try isnan(float(x)) catch e false end


Base.div(x::Sym, y) = convert(Sym, sympy.floor(project(x/convert(Sym,y))))

Base.rem(x::Sym, y) = x-Sym(y)*Sym(sympy.floor(project(x/y)))

Base.zero(x::Sym) = oftype(Sym, 0)
Base.zero{T<:Sym}(::Type{T}) = oftype(T,0)

Base.one(x::Sym) = oftype(Sym, 1)
Base.one{T<:Sym}(::Type{T}) = oftype(T, 1)

@doc """

Solve an expression for any zeros.

Examples: `solve(x^2 - x + 1)`

The `SymPy` docs say this about `solve`:

Note If solve returns [] or raises NotImplementedError, it
doesn’t mean that the equation has no solutions. It just means
that it couldn’t find any. Often this means that the solutions
cannot be represented symbolically. For example, the equation
`x=cos(x)` has a solution, but it cannot be represented
symbolically using standard functions.

In fact, solve makes no guarantees whatsoever about the completeness
of the solutions it finds. Much of solve is heuristics, which may find
some solutions to an equation or system of equations, but not all of
them.
        

""" -> 
function solve(ex::Sym, args...; kwargs...)
    a = sympy.solve(project(ex), map(project, args)...; kwargs...)

    ## Way too much work here to finesse into a nice enough output
    ## (Issue comes from solving without specifying variable when 2 or more variables in the expression
    if isa(a[1], Dict)
        d = Dict()
        for kv in a
            for (k,v) in kv
                cur = collect(keys(d))
                i = findfirst(cur, k)
                if i > 0
                    push!(d[cur[i]], v)
                else
                    d[k] = [v]
                end
            end
        end
        for (k,v) in d
            if length(v) == 1
                d[k] = v[1]
            end
        end
        if length(d) == 1
            collect(values(d))[1]
        else
            d
        end
    else
        Sym[v for v in a]
    end
end


@doc """

Solve a system of m equations with n unknowns, where the equations and
unknowns are passed as `Vector{Sym}`. If the unknowns are not
specified, all the free symbols will be used.

If succesful, returns an array of possible answers given by a dictionary. The dictionary is
of the form `string => Sym`, so to access the values, use `d[string(x)]` or `d["x"]`, but not `d[x]`.

The individual components of the array display more nicely than the array.

""" ->
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

@doc """
        Numerically solve for a zero of an expression.

        Examples: `solve(x^2 - x - 1, 1)`
""" ->             
nsolve(ex::Sym, x::Sym, x0::Number) = sympy.nsolve(project(ex), project(x), x0) |> float
nsolve(ex::Sym, x0::Number) =  sympy.nsolve(project(ex), x0) |> float
function nsolve{T <: Number}(ex::Vector{Sym}, x::Vector{Sym}, x0::Vector{T}; kwargs...)
    ans = sympy.nsolve(tuple(map(project,ex)...), tuple(map(project,x)...), tuple(x0...); kwargs...)
    ## ans is matrix object -- convert
    convert(Array{Sym}, sympy.Matrix(ans)) |> float
end
export nsolve

## dsolve
## Make a function argument, but munge arguments from Sym -> PyObject class
SymFunction(nm::Union(Symbol, String)) = (args...) -> Sym(sympy.Function(nm)(project(args)...))


@doc """

            Solve a differential equation.
            Examples:
            ```
            f = SymFunction("f")
            x = Sym("x")
            dsolve(diff(f(x), x) + f(x), f(x)) ## solve f'(x) + f(x) = 0
            dsolve(diff(f(x), x, x) + f(x), f(x)) ## solve f''(x) + f(x) = 0
            ```
""" ->            
dsolve(ex::Sym, fx::Sym) = sympy_meth(:dsolve, ex, fx)


@doc """

Create a piecewise defined function.

    Examples:
    ```
            p = piecewise((1, x < 1), (2, (1 <= x) ∨ (x <= 2)), (3, x > 2)) ## using ∨ and ∧ for & and or
            subs(p, x, 2) ## 2
            ```
""" ->                
function piecewise(args...)
    args = [map(project, x) for x in args]
    sympy.Piecewise(args...)
end

## special numbers
@doc "PI is a symbolic  π. Using `julia`'s `pi` will give round off errors." ->
const PI = Sym(sympy.pi)

@doc "E is a symbolic  `e`. Using `julia`'s `e` will give round off errors." ->
const E = Sym(sympy.exp(1))

@doc "I is a symbolic `im`" ->
const I = Sym(sympy.I)

@doc "oo is a symbolic infinity. Example: `integrate(exp(-x), x, 0, oo)`" ->
const oo = Sym(sympy.oo)
