{-# LANGUAGE GADTs #-}

-- ###################### --
-- #    BEGIN AXIOMS    # --
-- ###################### --

data Both a b = Both a b

data Gaia x where
    Import :: a -> (Gaia a)
    Bind :: Gaia a -> (a -> Gaia b) -> Gaia b

    -- Conditions
    CondForall :: Gaia a
    CondArrow :: Gaia a -> Gaia b -> Gaia (a -> b)

    -- Arrow Creation / Modus Ponens
    Arrow :: (a -> Gaia b) -> Gaia (a -> b)

    -- Merge Propositions
    Merge :: Gaia a -> Gaia b -> Gaia (Both a b)
    TakeL :: Gaia (Both a b) -> Gaia a
    TakeR :: Gaia (Both a b) -> Gaia b

instance Functor Gaia where
    fmap f x = undefined

instance Applicative Gaia where
    a <*> b = undefined
    pure a = undefined

instance Monad Gaia where
    a >>= f = undefined

-- ###################### --
-- #     END AXIOMS     # --
-- ###################### --

th1 :: Gaia ((a -> b) -> (b -> c) -> (a -> c))
th1 = Bind
    (Merge (Merge CondForall CondForall) CondForall)
    (\(Both (Both a b) c) -> undefined)
