module JewelRequire
## From JEWEL!
## 
## https://github.com/one-more-minute/Jewel.jl
# The Jewel.jl package is licensed under the MIT "Expat" License:
# Copyright (c) 2014: Mike Innes.


using Lazy

if VERSION < v"0.4-dev"
    Base.split(xs, x; keep=false) = split(xs, x, false)
end

function Base.require(s::ASCIIString)
  invoke(require, (String,), s)
  loadmod(s)
end

loaded(mod) = getthing(Main, mod) != nothing

const modlisteners = Dict{String,Vector{Function}}()

listenmod(f, mod) =
  loaded(mod) ? f() :
    modlisteners[mod] = push!(get(modlisteners, mod, Function[]), f)

loadmod(mod) =
  map(f->f(), get(modlisteners, mod, []))

importexpr(mod::Symbol) = Expr(:import, mod)
importexpr(mod::Expr) = Expr(:import, map(symbol, split(string(mod), "."))...)

macro require (mod, expr)
  quote
    listenmod($(string(mod))) do
      $(esc(Expr(:call, :eval, Expr(:quote, Expr(:block,
                                                 importexpr(mod),
                                                 expr)))))
    end
  end
end

macro lazymod (mod, path)
  quote
    function $(symbol(lowercase(string(mod))))()
      if !isdefined($(current_module()), $(Expr(:quote, mod)))
        includehere(path) = eval(Expr(:call, :include, path))
        includehere(joinpath(dirname(@__FILE__), $path))
        loadmod(string($mod))
      end
      $mod
    end
  end |> esc
end

# Qualified names → objects

function getthing(mod::Module, name::Vector{Symbol}, default = nothing)
  thing = mod
  for sym in name
    if isdefined(thing, sym)
      thing = thing.(sym)
    else
      return default
    end
  end
  return thing
end

getthing(name::Vector{Symbol}, default = nothing) =
  getthing(Main, name, default)

getthing(mod::Module, name::String, default = nothing) =
  name == "" ?
    default :
    @as _ name split(_, ".", keep=false) map(symbol, _) getthing(mod, _, default)

getthing(name::String, default = nothing) =
  getthing(Main, name, default)

getthing(::Nothing, default) = default
getthing(mod, ::Nothing, default) = default

# include_string with line numbers

function Base.include_string(s::String, fname::String, line::Integer)
  include_string("\n"^(line-1)*s, fname)
end

function Base.include_string(mod::Module, args...)
  eval(mod, :(include_string($(args...))))
end

# Get the current module for a file/pos

function getmodule(code, pos; filemod = nothing)
  codem = codemodule(code, pos)
  modstr = (codem != "" && filemod != nothing) ? "$filemod.$codem" :
           codem == "" ? filemod : codem
  getthing(modstr, Main)
end


end
