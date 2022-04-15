module Day3 where

import Data.Maybe -- Just a or Nothing

sumList list = if null list
  then 0
  else (head list) + (sumList (tail list))

-- countdown from x to 0, creating a list
countdown x = if x == 0
  then []
  else x : countdown (x - 1)

append xlist ylist = if null xlist
  then ylist
  else (head xlist) : append (tail xlist) ylist

sumPairList list = if null list
  then 0
  else fst (head list) + snd (head list) + sumPairList (tail list)

firsts list = if null list
  then []
  else fst (head list) : firsts (tail list)

sumPairList2 list1 list2 = sumPairList list1 + sumPairList list2

cylinder r h =
  let
    sideArea = 2 * pi * r * h
    topArea = pi * r^2
  in
    sideArea + 2 * topArea

countUpFrom1 x =
--creating a function in a function
  let
    count from to =
      if from == to
      then to:[]
      else from : count (from + 1) to
  in
    count 1 x

countUpFrom1' x =
  let
    count from =
      if from == x
      then x:[]
      else from : count (from + 1)
  in
    count 1

badMax list =
  if null list
  then 0 -- note, this is bad style
  else if null (tail list)
       then head list
       else if head list > badMax (tail list)
            then head list
            else badMax (tail list)

goodMax list =
  if null list
  then 0 -- bad style
  else if null (tail list)
       then head list
       else let
               result = goodMax (tail list)
            in
              if (head list) > result
              then (head list)
              else result

valOf x = case x of -- determine type of x
  (Just val) -> val
  (Nothing) -> error "can't take the value of nothing"

betterMax list =
  if null list
  then Nothing
  else
    let
      result = betterMax (tail list)
    in
      if isJust result && ((valOf result) > head list)
      then result
      else Just (head list)

sortPair pair = if (fst pair) < (snd pair)
  then pair
  else (snd pair, fst pair)

append' xlist ylist = if null xlist
  then ylist
  else (head xlist) : append' (tail xlist) ylist

-- declare a new variant type
data MyType = TwoInts Int Int | Spaghetti [Char] | Pizza deriving (Show)

myfun x = case x of
  TwoInts x 0 -> x * 20
  TwoInts x y -> x + y
  Spaghetti name -> 0
  Pizza -> 100
