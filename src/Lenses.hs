module Lenses (main) where

data Foo = Foo
    { bar :: (Int, Int)
    , baz :: Char
    }
    deriving (Show)

data Lens s a = Lens {view :: s -> a, set :: s -> a -> s}

barL :: Lens Foo (Int, Int)
barL = Lens bar $ \m x -> m{bar = x}

_2 :: Lens (a1, a2) a2
_2 = Lens snd $ \(a, _) x -> (a, x)

myTuple :: (Int, Int)
myTuple = (1, 1)

myFoo :: Foo
myFoo = Foo myTuple 'a'

main :: IO ()
main = do
    print $ view _2 myTuple
    print $ set _2 myTuple 2
