{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}

data Prop x where
    PropImportFunc :: (x -> y) -> Prop x -> Prop y
    PropModusPonens :: Prop (x -> y) -> Prop x -> Prop y
    PropImport :: x -> Prop x
    PropBind :: Prop x -> (x -> Prop y) -> Prop y
    PropNew :: Prop x

data Trace = Bind Trace Trace | Fmap Trace | Apply Trace Trace | Pure

data Gaia x = Gaia {
    prop :: Prop x,
    trace :: Trace
}

instance Functor Gaia where
    fmap f x = Gaia { prop = PropImportFunc f (prop x), trace = Fmap (trace x) }

instance Applicative Gaia where
    a <*> b = Gaia { prop = PropModusPonens (prop a) (prop b), trace = Apply (trace a) (trace b) }
    pure a = Gaia { prop = PropImport a, trace = Pure }

instance Monad Gaia where
    a >>= f = Gaia { prop = PropBind (prop a) (\x -> prop $ f undefined), trace = Bind (trace a) (trace $ f undefined) }

mp :: Gaia (a -> b) -> Gaia a -> Gaia b
mp = (<*>)

--th1 :: Gaia ((a -> b) -> (b -> c) -> (a -> c))