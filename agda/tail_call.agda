open import lib

-- factorial implementation (normal v.s. tail-call)
-- normal & public version
fact : ℕ → ℕ
fact 0 = 1
fact (suc n) = (suc n) * (fact n)

-- optimized version using tail recursive (tail call optimization)
fact' : ℕ → ℕ → ℕ
fact' 0 acc = acc
fact' (suc n) acc = fact' n ((suc n) * acc)

fact_t : ℕ → ℕ
fact_t n = fact' n 1

-- fibonacci implementation (normal v.s. tail-call)
-- normal & public version
fib : ℕ → ℕ
fib 0 = 0
fib 1 = 1
fib (suc (suc n)) = fib (suc n) + fib n

-- optimized version using tail recursive (tail call optimization)
fib' : ℕ → ℕ → ℕ → ℕ
fib' 0 x _ = x
fib' (suc n) x y = fib' n y (x + y)

fib_t : ℕ → ℕ
fib_t n = fib' n 0 1
