open import lib

_charAt_ : string → ℕ → maybe char
s charAt n = nth n (string-to-𝕃char s)

maybeToChar : maybe char → char
maybeToChar nothing = ' '
maybeToChar (just c) = c

reverseStr : string → string
reverseStr s = 𝕃char-to-string (reverse (string-to-𝕃char s))

𝔹toℕ : 𝔹 → ℕ
𝔹toℕ tt = 1
𝔹toℕ ff = 0

ℕto𝔹 : ℕ → 𝔹
ℕto𝔹 0 = ff
ℕto𝔹 _ = tt

_or'_ : ℕ → ℕ → ℕ
0 or' 0 = 0
0 or' 1 = 1
1 or' 1 = 1
1 or' 0 = 1
p or' q = 𝔹toℕ ((ℕto𝔹 p) || (ℕto𝔹 q))

_or_ : 𝕃 ℕ → 𝕃 ℕ → 𝕃 ℕ
[] or [] = []
[] or (y :: ys) = y :: ([] or ys)
(x :: xs) or [] = x :: (xs or [])
(x :: xs) or (y :: ys) = (x or' y) :: (xs or ys)

charℕ : char → ℕ
charℕ c = (toNat c) ∸ 48

𝕃char-to-𝕃ℕ : 𝕃 char → 𝕃 ℕ
𝕃char-to-𝕃ℕ [] = []
𝕃char-to-𝕃ℕ (x :: xs) = (charℕ x) :: 𝕃char-to-𝕃ℕ xs

string-to-𝕃ℕ : string → 𝕃 ℕ
string-to-𝕃ℕ s = 𝕃char-to-𝕃ℕ (string-to-𝕃char s)

