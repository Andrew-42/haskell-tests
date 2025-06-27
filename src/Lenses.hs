{-# LANGUAGE RankNTypes #-}

module Lenses (main) where

data Foo = Foo
    { bar :: (Int, Int)
    , baz :: Char
    }
    deriving (Show)

-- (a -> a) is a function that updates a field and is promoted to a function
-- that updates the structure (s -> s)
type Lens s a = forall f. (Functor f) => (a -> f a) -> s -> f s

newtype Identity a = Identity {runIdentity :: a}

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

newtype Const b a = Const {runConst :: b}

instance Functor (Const b) where
    fmap _ (Const b) = Const b

lens :: (s -> a) -> (s -> a -> s) -> Lens s a
lens sa sas afa s = sas s <$> afa (sa s)

barL :: Lens Foo (Int, Int)
barL = lens bar $ \m x -> m{bar = x}

_2 :: Lens (a1, a2) a2
_2 = lens snd $ \(a, _) x -> (a, x)

view :: Lens s a -> s -> a
view l s = runConst $ l Const s

set :: Lens s a -> a -> s -> s
set l a s = runIdentity $ l (const $ Identity a) s

myTuple :: (Int, Int)
myTuple = (1, 1)

myFoo :: Foo
myFoo = Foo myTuple 'a'

main :: IO ()
main = do
    print $ view (barL . _2) myFoo
    print $ set (barL . _2) 2 myFoo
