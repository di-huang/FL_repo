open import lib

init𝕍 : ∀{A : Set}{n : ℕ} → 𝕍 A (suc n) → 𝕍 A n
init𝕍 (x :: []) = []
init𝕍 (x :: y :: xs) = x :: init𝕍 (y :: xs)

last𝕍 : ∀{A : Set}{n : ℕ} → 𝕍 A (suc n) → A
last𝕍 (x :: []) = x
last𝕍 (x :: y :: xs) = last𝕍 (y :: xs)

filter𝕍 : ∀{A : Set}{n : ℕ} → (A → 𝔹) → 𝕍 A n → Σ ℕ (λ m → m ≤ n ≡ tt ∧ 𝕍 A m)
filter𝕍 p [] = 0 , refl , []
filter𝕍 p (x :: xs) with filter𝕍 p xs
... | (m , r , v) = if p x then (suc m , r , x :: v) else (m , {!!} , v)

filter𝕍' : ∀{A : Set}{n : ℕ} → (A → 𝔹) → 𝕍 A n → Σ ℕ (λ m → 𝕍 A m)
filter𝕍' p [] = 0 , []
filter𝕍' p (x :: xs) with filter𝕍' p xs
... | (n , v) = if p x then (suc n , x :: v) else (n , v)

2n-1 : ℕ → ℕ
2n-1 0 = 0
2n-1 1 = 1
2n-1 (suc (suc n)) = suc (suc (2n-1 (suc n)))

intersperse𝕍 : ∀{A : Set}{n : ℕ} → A → 𝕍 A n → 𝕍 A (2n-1 n)
intersperse𝕍 _ [] = []
intersperse𝕍 _ (x :: []) = x :: []
intersperse𝕍 i (x :: y :: xs) = x :: i :: intersperse𝕍 i (y :: xs)

m' : ℕ → ℕ → ℕ
m' 0 _ = 0
m' _ 0 = 0
m' (suc m) (suc n) = suc (m' m n)

take𝕍 : ∀{A : Set}{n : ℕ} → (m : ℕ) → 𝕍 A n → 𝕍 A (m' m n)
take𝕍 0 [] = []
take𝕍 0 (x :: xs) = []
take𝕍 (suc m) [] = []
take𝕍 (suc m) (x :: xs) = x :: take𝕍 m xs
