{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}

module Comonads where

import Control.Comonad
import Control.Concurrent (threadDelay)
import System.IO (hFlush, stdout)

data Zipper a = Zipper [a] a [a]

instance (Show a) => Show (Zipper a) where
    show :: (Show a) => Zipper a -> String
    show (Zipper lz z rz) = unwords $ map show $ take 5 lz ++ [z] ++ take 5 rz

instance Functor Zipper where
    fmap :: (a -> b) -> Zipper a -> Zipper b
    fmap f (Zipper lz x rz) = Zipper (f <$> lz) (f x) (f <$> rz)

instance Comonad Zipper where
    extract :: Zipper a -> a
    extract (Zipper _ x _) = x
    duplicate :: Zipper a -> Zipper (Zipper a)
    duplicate z = Zipper (tail $ iterate leftZ z) z (tail $ iterate rightZ z)

leftZ :: Zipper a -> Zipper a
leftZ (Zipper (l : lz) x rz) = Zipper lz l (x : rz)
leftZ _ = error "The list should be infinite."

rightZ :: Zipper a -> Zipper a
rightZ (Zipper lz x (r : rz)) = Zipper (x : lz) r rz
rightZ _ = error "The list should be infinite."

setZ :: a -> Zipper a -> Zipper a
setZ v (Zipper lz _ rz) = Zipper lz v rz

takeN :: Int -> Zipper a -> [a]
takeN n (Zipper _ x rz) = x : take (n - 1) rz

newtype Grid a = Grid (Zipper (Zipper a))

instance (Show a) => Show (Grid a) where
    show :: (Show a) => Grid a -> String
    show (Grid (Zipper lz z rz)) = unlines $ map show $ take 5 lz ++ [z] ++ take 5 rz

instance Functor Grid where
    fmap :: (a -> b) -> Grid a -> Grid b
    fmap f (Grid g) = Grid $ fmap (fmap f) g

instance Comonad Grid where
    extract :: Grid a -> a
    extract (Grid g) = extract . extract $ g
    duplicate :: Grid a -> Grid (Grid a)
    duplicate g = Grid $ fmap horizontal (vertical g)
      where
        horizontal g' = Zipper (tail $ iterate left g') g' (tail $ iterate right g')
        vertical g' = Zipper (tail $ iterate up g') g' (tail $ iterate down g')

up, down, left, right :: Grid a -> Grid a
up (Grid g) = Grid $ leftZ g
down (Grid g) = Grid $ rightZ g
left (Grid g) = Grid $ fmap leftZ g
right (Grid g) = Grid $ fmap rightZ g

set :: a -> Grid a -> Grid a
set v (Grid g) = Grid $ fmap (setZ v) g

toGrid :: a -> [[a]] -> Grid a
toGrid v ls = case ls of
    [] -> Grid $ Zipper (repeat defaultRow) defaultRow (repeat defaultRow)
    [x] -> Grid $ Zipper (repeat defaultRow) (toZipper x) (repeat defaultRow)
    (x : xs) -> Grid $ Zipper (repeat defaultRow) (toZipper x) (map toZipper xs ++ repeat defaultRow)
  where
    toZipper [] = error "Row must not be empty."
    toZipper row = Zipper (repeat v) (head row) (tail row ++ repeat v)
    defaultRow = Zipper (repeat v) v (repeat v)

data GridBorder
    = GridBorder {leftX :: Int, rightX :: Int, topY :: Int, bottomY :: Int}

moveToCol :: Int -> Grid a -> Grid a
moveToCol dx g
    | dx == 0 = g
    | dx > 0 = moveToCol (dx - 1) (right g)
    | otherwise = moveToCol (dx + 1) (left g)

moveToRow :: Int -> Grid a -> Grid a
moveToRow dy g
    | dy == 0 = g
    | dy > 0 = moveToRow (dy - 1) (up g)
    | otherwise = moveToRow (dy + 1) (down g)

moveTo :: (Int, Int) -> Grid a -> Grid a
moveTo (dx, dy) = moveToCol dx . moveToRow dy

getGrid :: GridBorder -> Grid a -> [[a]]
getGrid (GridBorder{..}) g = map (takeN nCols) rows
  where
    (Grid startG) = moveTo (leftX, topY) g
    nCols = abs (rightX - leftX)
    nRow = abs (topY - bottomY)
    rows = takeN nRow startG

-- * Game of Life

data Cell = Dead | Alive deriving (Eq, Show)

rule :: Grid Cell -> Cell
rule g = case extract g of
    Dead -> if aliveNeighbors == 3 then Alive else Dead
    Alive -> if aliveNeighbors `elem` [2, 3] then Alive else Dead
  where
    neighbors = [up, down, left, right, up . right, up . left, down . right, down . left]
    aliveNeighbors = length . filter (== Alive) . map (extract . ($ g)) $ neighbors

step :: Grid Cell -> Grid Cell
step = extend rule

blinker :: [[Cell]]
blinker = [[Alive, Alive, Alive]]

glider :: [[Cell]]
glider =
    [ [Dead, Alive, Dead]
    , [Dead, Dead, Alive]
    , [Alive, Alive, Alive]
    ]

stringGrid :: [[Cell]] -> String
stringGrid t = unlines $ map (unwords . map toChar) t
  where
    toChar c = if c == Alive then "x" else "."

evolutions :: Int -> Int -> GridBorder -> Grid Cell -> IO ()
evolutions _ 0 _ _ = return ()
evolutions fin n gb g = do
    cursorStart
    putStrLn $ "Evolution step: " ++ show (fin - n + 1) ++ " / " ++ show fin
    putStrLn $ stringGrid (getGrid gb g)
    putStrLn ""
    hFlush stdout
    sleep
    let nextG = step g
    evolutions fin (n - 1) gb nextG

runGame :: IO ()
runGame = do
    clearScreen
    let n = 35
    let g = toGrid Dead glider
    let gridBorder = GridBorder (-2) 10 3 (-9)
    evolutions n n gridBorder g

cursorStart :: IO ()
cursorStart = putStr "\ESC[H"

clearScreen :: IO ()
clearScreen = putStr "\ESC[2J\ESC[H"

sleep :: IO ()
sleep = threadDelay 100000 -- 0.5 second
