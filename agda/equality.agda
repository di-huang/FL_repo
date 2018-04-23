open import lib

-- equality test for Nat list
_=[ℕ]_ : 𝕃 ℕ → 𝕃 ℕ → 𝔹
[] =[ℕ] [] = tt
[] =[ℕ] (y :: ys) = ff
(x :: xs) =[ℕ] [] = ff
(x :: xs) =[ℕ] (y :: ys) with x =ℕ y
... | tt = xs =[ℕ] ys
... | ff = ff

-- equality test for Bool list
_=[𝔹]_ : 𝕃 𝔹 → 𝕃 𝔹 → 𝔹
[] =[𝔹] [] = tt
[] =[𝔹] (y :: ys) = ff
(x :: xs) =[𝔹] [] = ff
(x :: xs) =[𝔹] (y :: ys) with x iff y
... | tt = xs =[𝔹] ys
... | ff = ff
