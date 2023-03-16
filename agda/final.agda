module final where

data ℕ : Set where
  zero : ℕ
  suc : ℕ → ℕ

one : ℕ
one = suc zero

two : ℕ
two = suc one

three : ℕ
three = suc two

_+_ : ℕ → ℕ → ℕ
zero + y = y
suc x + y = suc (x + y)

data _≡_ {A : Set} (x : A) : A → Set where
  refl : x ≡ x

infix 1 _≡_
infixr 6 _+_

_ : one + two ≡ three
_ = refl

data _≤_ : ℕ → ℕ → Set where
  z≤n : {n : ℕ} → zero ≤ n
  s≤s : {x y : ℕ} → x ≤ y → suc x ≤ suc y


2≤3 : two ≤ three
2≤3 = s≤s (s≤s z≤n)

bigger : ℕ → ℕ
bigger = suc

bigger-is-≤ : (n : ℕ) → n ≤ bigger n
bigger-is-≤ zero = z≤n
bigger-is-≤ (suc n) = s≤s (bigger-is-≤ n)

record BiggerProof (n : ℕ) : Set where
  field
    k : ℕ
    k-bigger : n ≤ k

mkBigger : (n : ℕ) → BiggerProof n
BiggerProof.k (mkBigger n) = bigger n
BiggerProof.k-bigger (mkBigger n) = bigger-is-≤ n

record Σ (A : Set) (f : A → Set) : Set where
  field
    fst : A
    snd : f fst

Bigger-to-Σ : {n : ℕ} → BiggerProof n → Σ ℕ (λ k → n ≤ k)
Σ.fst (Bigger-to-Σ x) = BiggerProof.k x
Σ.snd (Bigger-to-Σ x) = BiggerProof.k-bigger x

data List (A : Set) : Set where
  [] : List A
  _∷_ : A → List A → List A

infixr 3 _∷_

private variable
  A : Set
  a : A
  s₁ s₂ : List A

data SubSeq : List A → List A → Set where
  []∈s  : SubSeq [] s₂
  match : SubSeq s₁ s₂ → SubSeq (a ∷ s₁) (a ∷ s₂)
  skip  : SubSeq s₁ s₂ → SubSeq s₁ (a ∷ s₂)

data ⊥ : Set where

¬_ : Set → Set
¬ P = P → ⊥

data Dec (P : Set) : Set where
  yes :   P → Dec P
  no  : ¬ P → Dec P

data Bool : Set where
  true false : Bool

does : {P : Set} → Dec P → Bool
does (yes x) = true
does (no x) = false

cong : {A B : Set} → {x y : A} → (f : A → B) → x ≡ y → f x ≡ f y
cong f refl = refl

suc-injective : {x y : ℕ} → suc x ≡ suc y → x ≡ y
suc-injective refl = refl

_≟_ : (x y : ℕ) → Dec (x ≡ y)
zero ≟ zero = yes refl
zero ≟ suc y = no λ { () }
suc x ≟ zero = no λ { () }
suc x ≟ suc y with x ≟ y
... | yes x=y = yes (cong suc x=y)
... | no  x≠y = no λ { p → x≠y (suc-injective p) }

_ : Dec (two ≡ one + one)
_ = yes refl

_ : Dec (two ≡ two + one)
_ = no λ ()

import Agda.Builtin.Char

_ : SubSeq ('o' ∷ 'o' ∷ []) ('l' ∷ 'l' ∷ 'o' ∷ 'w' ∷ 'o' ∷ 'r' ∷ [])
_ = skip (skip (match (skip (match []∈s))))

liar : {x : A} (xs ys : List A) → ¬ SubSeq xs ys → SubSeq (x ∷ xs) ys → ⊥
liar xs (y ∷ ys) ¬z (match p) = ¬z (skip p)
liar xs (y ∷ ys) ¬z (skip p) = liar xs ys (λ z → ¬z (skip z)) p

subseq : (_≟_ : (x y : A) → Dec (x ≡ y)) → (xs ys : List A) → Dec (SubSeq xs ys)
subseq _≟_ [] ys = yes []∈s
subseq _≟_ (x ∷ xs) [] = no (λ ())
subseq _≟_ (x ∷ xs) (y ∷ ys) with x ≟ y
subseq _≟_ (x ∷ xs) (y ∷ ys) | yes refl with subseq _≟_ xs ys
... | yes sub-xy = yes (match sub-xy)
... | no ¬sub-xy = no λ { (match p) → ¬sub-xy p
                        ; (skip p) → liar xs ys ¬sub-xy p
                        }
subseq _≟_ (x ∷ xs) (y ∷ ys) | no x≠y with subseq _≟_ (x ∷ xs) ys
... | yes sub-xy = yes (skip sub-xy)
... | no ¬sub-xsy = no λ { (match p) → x≠y refl
                         ; (skip p) → ¬sub-xsy p }

