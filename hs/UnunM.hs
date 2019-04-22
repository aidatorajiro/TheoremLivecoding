data Prop = Var String | Forall String Prop | (:->) Prop Prop deriving (Eq, Show)

data Proof = Proof Prop deriving (Eq, Show)

-- Axioms

-- | destruction func for Arrow
dArrow :: Proof -> Proof -> Maybe Proof
dArrow (Proof (a :-> b)) (Proof c) = if a == c then Just $ Proof b else Nothing
dArrow _ _ = Nothing

-- | construction func for Arrow
cArrow :: Prop -> (Proof -> Prop -> Proof) -> Maybe Proof
cArrow a@(p :-> q) f = if f (Proof p) q == Proof q then Just $ Proof a else Nothing
cArrow _ _ = Nothing

-- | destruction func for Forall
dForall :: Proof -> Prop -> Maybe Proof
dForall (Proof (Forall s p)) q = let Forall _ r = (replaceVar s p q) in Just $ Proof r
dForall _ _ = Nothing

-- | construction func for Forall
cForall :: Prop -> (Prop -> Proof) -> Maybe Proof
cForall a@(Forall _ p) f = if f p == Proof p then Just $ Proof a else Nothing
cForall _ _ = Nothing

-- Utils

-- | Replace `var` into `value` in `base`
replaceVar :: String -> Prop -> Prop -> Prop
replaceVar var base@(Var s) value = if var == s then value else base
replaceVar var base@(Forall s p) value = Forall s (replaceVar var p value)
replaceVar var base@(p :-> q) value = (replaceVar var p value) :-> (replaceVar var q value)

-- | get corresponding Prop from Proof
getProp :: Proof -> Prop
getProp (Proof p) = p

-- Proof Monad

data ProofState = ProofState { currentState :: Proof, proofLog :: [String] }



-- !!! PROOF !!!

