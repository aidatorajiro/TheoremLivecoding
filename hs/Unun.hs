data Prop = Var String | Forall String Prop | (:->) Prop Prop deriving (Eq, Show)

data Proof = Proof Prop deriving (Eq, Show)

-- | Substitute `value` as `var` in `base`
substitute :: String -> Prop -> Prop -> Prop
substitute var base@(Var s) value = if var == s then value else base
substitute var base@(Forall s p) value = Forall s (substitute var p value)
substitute var base@(p :-> q) value = (substitute var p value) :-> (substitute var q value)

dArrow :: Proof -> Proof -> Proof
dArrow (Proof (a :-> b)) (Proof c) = if a == c then Proof b else error "mismatch at dArrow"
dArrow _ _ = error "mismatch at dArrow"

cArrow :: Prop -> (Proof -> Proof) -> Proof
cArrow a@(p :-> q) f = if f (Proof p) == (Proof q) then Proof a else error "mismatch at cArrow"
cArrow _ _ = error "mismatch at cArrow"

dForall :: Proof -> Prop -> Proof
dForall (Proof (Forall s p)) q = Proof (substitute s p q)
dForall _ _ = error "mismatch at dForall"

cForall :: Prop -> Proof -> Proof
cForall a@(Forall _ p) (Proof q) = if p == q then Proof a else error "mismatch at cForall"
cForall _ _ = error "mismatch at cForall"

getProp :: Proof -> Prop
getProp (Proof p) = p

thm1 :: Prop
thm1 = Forall "a" (Var "a" :-> Var "a")

prf1 :: Proof
prf1 = cForall thm1 (cArrow (Var "a" :-> Var "a") (\a -> a))

thm2 :: Prop
thm2 = 
    Forall "a" (
        Forall "b" (
            Forall "c" (
                (Var "a" :-> Var "b") :-> ((Var "b" :-> Var "c") :-> (Var "a" :-> Var "c"))
            )
        )
    )

