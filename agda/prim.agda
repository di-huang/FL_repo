open import lib

-- primitively, n ≡ zero + n , n ≡ 0 + n
prim₀ : ∀{n : ℕ} → n ≡ zero + n
prim₀ = refl

-- primitively, zero + n ≡ n , 0 + n ≡ n
prim₀' : ∀{n : ℕ} → zero + n ≡ n
prim₀' = refl

-- need to prove: n + zero ≡ n , n + 0 ≡ n
lem₀ : ∀{n : ℕ} → n + zero ≡ n
lem₀ {zero} = refl
lem₀ {suc n} rewrite lem₀ {n} = refl

-- need to prove: n ≡ n + zero , n ≡ n + 0
lem₀' : ∀{n : ℕ} → n ≡ n + zero
lem₀' {zero} = refl
lem₀' {suc n} rewrite lem₀ {n} = refl

-- primitive
prim₁ : ∀{n m : ℕ} → suc (n + m) ≡ (suc n) + m 
prim₁ = refl


