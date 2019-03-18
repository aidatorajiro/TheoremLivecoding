{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}

data Nat where
    Z :: Nat
    S :: Nat -> Nat

data Prop where
    (:->) :: Prop -> Prop -> Prop
    NatProp :: Nat -> Prop

--mp :: (a :-> b) :-> (b :-> c) :-> (a :-> c)
