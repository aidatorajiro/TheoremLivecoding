data Prop = Var String | Forall String Prop | (:->) Prop Prop deriving (Eq, Show)

data Proof = UnProof Prop deriving (Eq, Show)

-- substitute `value` as `var` in `base`
substitute :: String -> Prop -> Prop -> Prop
substitute var base@(Var s) value = if var == s then value else base

dArrow :: Proof -> Proof -> Proof
dArrow (UnProof (a :-> b)) (UnProof c) = if a == c then UnProof b else error "mismatch at dArrow"
dArrow _ _ = error "mismatch at dArrow"

cArrow :: Prop -> (Proof -> Proof) -> Proof
cArrow a@(p :-> q) f = if f (UnProof p) == (UnProof q) then UnProof a else error "mismatch at cArrow"
cArrow _ _ = error "mismatch at cArrow"

dForall :: Proof -> Prop -> Proof
dForall (UnProof (Forall s p)) q = UnProof (substitute s p q)
dForall _ _ = error "mismatch at dForall"

cForall :: Prop -> Proof -> Proof
cForall a@(Forall _ p) (UnProof q) = if p == q then UnProof a else error "mismatch at cForall"
cForall _ _ = error "mismatch at cForall"

getProp :: Proof -> Prop
getProp (UnProof p) = p

thm1 :: Prop
thm1 = Forall "a" (Var "a" :-> Var "a")

prf1 :: Proof
prf1 = cArrow thm1 (\a ->
        cArrow ((getProp a) :-> (getProp a)) (\_ -> a)
    )

thm2 :: Prop
thm2 = 
    Forall "a" (
        Forall "b" (
            Forall "c" (
                (Var "a" :-> Var "b") :-> ((Var "b" :-> Var "c") :-> (Var "a" :-> Var "c"))
            )
        )
    )

