{-# LANGUAGE Rank2Types #-}

module Simple2 where

-- ###################### --
-- #    BEGIN AXIOMS    # --
-- ###################### --

data Prop a = Prop a
data Gaia a = Gaia (GaiaState -> (a, GaiaState))
type GaiaState = [String]

state :: String -> GaiaState -> GaiaState
state s l = s : l

mp :: Prop (a -> b) -> Prop a -> Gaia (Prop b)
mp (Prop ab) (Prop a) = Gaia (\l -> (Prop (ab a), state "mp" l))

arrow :: (a -> Prop b) -> Prop (a -> b)
arrow f = Prop (\a -> let (Prop b) = f a in b)

forall :: Gaia (Prop a)
forall = Gaia (\l -> (Prop undefined, state "forall" l))

-- ###################### --
-- #     END AXIOMS     # --
-- ###################### --

