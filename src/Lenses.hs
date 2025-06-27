module Lenses (main) where

data Foo = Foo
    { bar :: (Int, Int)
    , baz :: Char
    }
    deriving (Show)

getBar :: Foo -> (Int, Int)
getBar = bar

setBar :: Foo -> (Int, Int) -> Foo
setBar s x = s{bar = x}

get_2 :: (a, b) -> b
get_2 = snd

set_2 :: (a, b) -> b -> (a, b)
set_2 (a, _) x = (a, x)

myTuple :: (Int, Int)
myTuple = (1, 1)

myFoo :: Foo
myFoo = Foo myTuple 'a'

main :: IO ()
main = pure ()
