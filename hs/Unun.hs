data Prop = Var String | Forall String Prop | (:->) Prop Prop deriving (Eq, Show)

data Proof = Proof Prop deriving (Eq, Show)

-- Axioms

-- | destruction func for Arrow
dArrow :: Proof -> Proof -> Proof
dArrow (Proof (a :-> b)) (Proof c) = if a == c then Proof b else error "mismatch at dArrow"
dArrow _ _ = error "mismatch at dArrow"

-- | construction func for Arrow
cArrow :: Prop -> (Proof -> Prop -> Proof) -> Proof
cArrow a@(p :-> q) f = if f (Proof p) q == Proof q then Proof a else error "mismatch at cArrow"
cArrow _ _ = error "mismatch at cArrow"

-- | destruction func for Forall
dForall :: Proof -> Prop -> Proof
dForall (Proof (Forall s p)) q = let Forall _ r = (replaceVar s p q) in Proof r
dForall _ _ = error "mismatch at dForall"

-- | construction func for Forall
cForall :: Prop -> (Prop -> Proof) -> Proof
cForall a@(Forall _ p) f = if f p == Proof p then Proof a else error "mismatch at cForall"
cForall _ _ = error "mismatch at cForall"

-- Utils

-- | Replace `var` into `value` in `base`
replaceVar :: String -> Prop -> Prop -> Prop
replaceVar var base@(Var s) value = if var == s then value else base
replaceVar var base@(Forall s p) value = Forall s (replaceVar var p value)
replaceVar var base@(p :-> q) value = (replaceVar var p value) :-> (replaceVar var q value)

-- | get corresponding Prop from Proof
getProp :: Proof -> Prop
getProp (Proof p) = p

-- !!! PROOF !!!

thm1 :: Prop
thm1 = Forall "a" (Var "a" :-> Var "a")

prf1 :: Proof
prf1 = cForall thm1 (\p -> cArrow p (\a _ -> a))

thm2 :: Prop
thm2 = 
    Forall "a" (
        Forall "b" (
            Forall "c" (
                (Var "a" :-> Var "b") :-> ((Var "b" :-> Var "c") :-> (Var "a" :-> Var "c"))
            )
        )
    )

prf2 :: Proof
prf2 = 
    cForall thm2 (\p ->
        cForall p (\q ->
            cForall q (\r ->
                cArrow r (\ab bcac ->
                    cArrow bcac (\bc ac ->
                        cArrow ac (\a _ ->
                            dArrow bc (dArrow ab a)
                        )
                    )
                )
            )
        )
    )

thm3 :: Prop
thm3 = Forall "a" (Var "a") :-> Forall "b" (Var "b")

prf3 :: Proof
prf3 = cArrow thm1 (\fa fb -> cForall fb (\b -> dForall fa b))