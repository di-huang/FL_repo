open import lib

imp≡tt : ∀ b → tt imp b ≡ tt → b ≡ tt
imp≡tt tt p = refl
imp≡tt ff p = p

imp≡tt' : ∀ b → tt imp b ≡ tt → b ≡ tt
imp≡tt' tt p = refl
imp≡tt' ff ()

imp≡ff : ∀ b → tt imp b ≡ ff → b ≡ ff
imp≡ff tt p = p
imp≡ff ff p = refl

imp≡ff' : ∀ b → tt imp b ≡ ff → b ≡ ff
imp≡ff' tt ()
imp≡ff' ff p = refl



