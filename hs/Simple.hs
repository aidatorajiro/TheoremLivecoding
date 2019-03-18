{-# LANGUAGE Rank2Types #-}

module Simple where

import Control.Applicative

-- ###################### --
-- #    BEGIN AXIOMS    # --
-- ###################### --

data Haskell x = Haskell x

data Prop = Bind Prop Prop | CondForall | CondArrow Prop Prop | Axiom | Import | MP Prop Prop

-- | Infer whether given prop is true or false. If given prop is false, return String which contains an error message. If true, return Nothing. 
infer :: Prop -> Maybe String
-- ingore lhs of Bind
infer (Bind _ x) = infer x
-- No substitution to qualification variables
infer (MP CondForall _) = Just "Illegal substitution"
-- Modus Ponens
infer (MP x y) = infer x <|> infer y
-- Axioms and Conditions are always true
infer Axiom = Nothing
infer (CondArrow _ _) = Nothing
infer CondForall = Nothing
infer Import = Nothing

data Gaia x = Gaia {
    getHaskell :: Haskell x,
    getProp :: Prop
} | GaiaError String

mp :: Gaia (a -> b) -> Gaia a -> Gaia b
mp (Gaia (Haskell f) pf) (Gaia (Haskell a) pa) = 
    let prop = (MP pf pa)
     in case infer prop of
        Just e -> GaiaError e
        Nothing -> Gaia (Haskell $ f a) prop

mp (GaiaError s) _ = GaiaError s
mp _ (GaiaError t) = GaiaError t

forall :: Gaia a
forall = Gaia (Haskell undefined) CondForall

arrow :: Gaia a -> Gaia b -> Gaia (a -> b)
arrow (Gaia _ pa) (Gaia _ pb) = Gaia (Haskell undefined) (CondArrow pa pb)
arrow (GaiaError s) _ = GaiaError s
arrow _ (GaiaError t) = GaiaError t

instance Functor Gaia where
    fmap f (Gaia (Haskell a) pa) = Gaia (Haskell $ f a) (MP Import pa)
    fmap _ (GaiaError s) = GaiaError s

instance Applicative Gaia where
    (<*>) = mp
    pure a = Gaia (Haskell a) Import

instance Monad Gaia where
    (Gaia (Haskell a) pa) >>= f = case f a of
        (Gaia hb pb) -> Gaia hb (Bind pa pb)
        GaiaError s -> GaiaError s
    (GaiaError s) >>= _ = GaiaError s

-- ###################### --
-- #     END AXIOMS     # --
-- ###################### --

thm1 :: Gaia ((a -> b) -> (b -> c) -> (a -> c))
thm1 = do
    a <- forall -- a :: a
    b <- forall -- b :: b
    c <- forall -- c :: c
    ab <- arrow a b -- ab :: a -> b
    bc <- arrow b c -- bc :: b -> c
