module Class10 where

data Point = Point Float Float deriving (Show, Read)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

area (Circle _ radius) = pi * radius^2
area (Rectangle (Point x1 y1) (Point x2 y2)) = (x2 - x1) * (y2 - y1)

data DoW = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Ord, Show, Enum, Bounded) -- Eq included

-- inheriting?
instance Eq DoW where
  Monday == Monday = True
  Tuesday == Tuesday = True
  Wednesday == Wednesday = True
  Thursday == Thursday = True
  Monday == Tuesday = True -- order matters; M == T does not mean T == M
  _ == _ = False -- everything else is false
  
data AntState a = Sleeping | Eating | Foraging | Biting | Building | Carrying a

instance (Eq a) => Eq (AntState a) where
  Sleeping == Sleeping = True
  Eating == Eating = True
  Foraging == Foraging = True
  Biting == Biting = True
  Building == Building = True
  Eating == Biting = True
  Biting == Eating = True
  Carrying x == Carrying y = x == y
  _ == _ = False
  
instance (Show a) => Show (AntState a) where
  show Sleeping = "aw, isn't it cute"
  show Eating = "hope it's not trying to get you!"
  show Foraging = "looking for stuff"
  show Biting = "ouch!"
  show Building = "probably working with Bob"
  show (Carrying x) = "the ant is carrying " ++ (show x)
  
-- -- definition of functor
-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b
--   
-- instance Functor [] where -- defines functor over list type; this code will not work
--   fmap = map
  
-- what
-- instance Functor (Maybe a) where
--   fmap f (Just x) = Just (f x)
--   fmap f Nothing = Nothing

data BTree a = Empty | Leaf a | Node a (BTree a) (BTree a) deriving (Show)

-- functor is a typeclass that can be mapped
instance Functor BTree where
  fmap f Empty = Empty
  fmap f (Leaf val) = Leaf (f val)
  fmap f (Node val left right) = Node (f val) (fmap f left) (fmap f right)
  
-- main = do
--   putStrLn "Enter your name: "
--   name <- getLine
--   putStrLn ("Hello " ++ name)

-- main = do
--   _ <- putStr "Enter your name:"
--   name <- getLine
--   putStrLn ("Hello " ++ name)

-- foo = do
--   let
--     x = y
--     y = 6
--   putStrLn (show (x + y))
  
main = do
  line <- getLine
  if null line
  then return ()
  else
    do
      putStrLn $ reverseWords line
      main
      
reverseWords = unwords . map reverse . words

-- ghc --make filename

-- main = do
--   return 5
--   return "Hello there"
--   return ()
--   putStrLn "hi"  

-- putStr
-- putChar
-- print
-- getChar
-- getLine

--import Control.Monad
-- main = do
--   c <- getChar
--   when (c /= ' ') $ do
--     putChar c
--     main

-- sequence
-- mapM
-- mapM_
-- forever
-- forM

-- main = do
--   num <- forM [1..10] (\n -> print n) -- executes loop 10 times
--   return ()

-- file IO
-- import System.IO

-- main = do
--   handle <- openFile "pl9.hs" ReadMode
--   contents <- hGetContents handle
--   putStr contents
--   hClose handle

-- getProgName
-- getArgs
  
  