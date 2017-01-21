## http://docs.sympy.org/dev/modules/ntheory.html?highlight=ntheory#sympy.ntheory.factor_
ntheory_sympy_methods = (
                         :sieve,
                         :prime, :primepi,
                         :isprime, # import
                         :nextprime, :prevprime, :primorial,
                         :trailing,
                         :multiplicity, :perfect_power,
                         :pollard_rho, :pollard_pm1,
                         :factorint, :primefactors, :divisors, :divisor_count,
                         :totient,
                         :binomial_coefficients, :binomial_coefficients_list,
                         :multinomial_coefficients,
                         :npartitions,
                         :n_order, :is_primitive_root, :is_quad_residue,
                         :legendre_symbol, :jacobi_symbol
                         )

"""

[primerange](http://docs.sympy.org/dev/modules/ntheory.html): Generate all prime numbers in the range [a, b).

This is a replication of `primes` to which we dispatch.

"""
function primerange(a::Sym,b)
    a = max(2,convert(Int, a))
    b = max(a+1, convert(Int, b))
    primes(a,b)
end
export primerange
    
