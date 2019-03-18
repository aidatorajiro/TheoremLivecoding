{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}

data Prop = IntProp Int | Forall (Prop -> Prop) | (:->) Prop Prop deriving (Eq, Show)

instance Eq (Prop -> Prop) where
    _ == _ = False

instance Show (Prop -> Prop) where
    show x = show (x (IntProp 0))

type Log = [String]
data Proof x = Proof ((Prop, Int, Log) -> (x, Prop, Int, Log))

popForall :: Proof ()
popForall = Proof (\(Forall x, i, l) -> ((), x (IntProp i), i + 1, "pop forall" : l))

popCondition :: Proof Prop
popCondition = Proof (\(a :-> b, i, l) -> (a, b, i, "pop condition" : l))

get :: Proof Prop
get = Proof (\(p, i, l) -> (p, p, i, "get current" : l))

mp :: Prop -> Prop -> Proof Prop
mp (a :-> b) a' = Proof (\(p, i, l) -> if a == a' then (b, p, i, "modus ponens" : l) else undefined)

instance Functor Proof where
    fmap f (Proof g) = Proof (\(p, i, l) -> 
        let (a, p', i', l') = g (p, i, l)
         in (f a, p', i', "functor" : l')
        )

instance Applicative Proof where
    (Proof f) <*> (Proof g) = Proof (\(p, i, l) ->
        let (ab, p', i', l') = f (p, i, l)
            (a, p'', i'', l'') = g (p', i', l')
         in (ab a, p'', i'', "applicative" : l'')
        )
    
    pure a = Proof (\(p, i, l) -> (a, p, i, l))

instance Monad Proof where
    (Proof f) >>= g = Proof (\(p, i, l) ->
        let (a, p', i', l') = f (p, i, l)
            (Proof h) = g a
            (b, p'', i'', l'') = h (p', i', l')
         in (b, p'', i'', "monad bind" : l'')
        )

thm1 :: Prop
thm1 = 
    Forall (\a ->
        Forall (\b ->
            Forall (\c ->
                (a :-> b) :-> ((b :-> c) :-> a)
            )
        )
    )

prf1 :: Proof Prop
prf1 = do
    popForall -- pop forall a.
    popForall -- pop forall b.
    popForall -- pop forall c.
    ab <- popCondition
    bc <- popCondition
    a <- get
    b <- mp ab a
    c <- mp bc b
    return c
