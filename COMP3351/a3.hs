module A3 where
import Data.List
import Data.Char

-- onlyLowercase
-- takes string list and returns string list with only strings in input with a lowercase letter

onlyLowercase list = filter (\x -> isLower (head x)) list

-- longestString
-- takes string list and returns longest string in the list
-- returns "" if list is empty
-- returns first one if there is a tie
-- use foldl (anonymous function) (param one) (param two)

longestString [] = ""
longestString (x:rest) = foldl (\x y -> if (length x) >= (length y) then x else y) x rest

-- longestString'
-- takes a string list and returns the longest string in the list
-- returns "" if list is empty
-- returns last one in the case of a tie
-- use fold l

longestString' [] = ""
longestString' (x:rest) = foldl (\x y -> if (length x) > (length y) then x else y) x rest

-- longestStringHelper
-- takes a function f1 and a list of strings
-- returns a string
-- f1 takes two ints and returns a bool
-- if function is like > then it has the same behavior as longestString

longestStringHelper function [] = ""
longestStringHelper function list =
  let
    longestStringLength currLongestString currString = 
      if function (length currLongestString) (length currString)
      then currLongestString
      else currString
  in
    foldl longestStringLength "" list
  
-- longestString3
-- same behavior as longestString
-- uses longestStringHelper

longestString3 [] = ""
longestString3 list = longestStringHelper (>=) list

-- longestString4
-- same behavior as longestString'
-- uses longestStringHelper

longestString4 [] = ""
longestString4 list = longestStringHelper (>) list

-- longestLowercase
-- takes a string list and returns the longest string that begins with a lowercase letter or "" if no such strings
-- if tie, return first one

longestLowercase list = (longestString . onlyLowercase) list

-- revStringRev
-- takes a string and returns the string in reverse order and every uppercase letter is lowercase

revStringRev [] = ""
revStringRev list =  
  let
    toLC [] = ""
    toLC (x:xs) = (toLower x) : toLC xs
  in
    reverse (toLC list)
 
-- firstAnswer
-- first argument is applied to elements of second argument (list) until it returns Just v 
-- if first argument returns nothing, then Nothing

firstAnswer predicate [] = Nothing
firstAnswer predicate (x:xs) = 
  case (predicate x) of
    Just v -> Just v
    Nothing -> firstAnswer predicate xs
    
-- allAnswers
-- allAnswers predicate [] = Nothing
-- allAnswers predicate (x:xs) = 
--   let
--     aux predicate acc (x:xs) = 
--       case (predicate x) of
--       Nothing -> Nothing
--       Just v -> (aux predicate v xs) ++ [v]
--   in
--     aux predicate 0 (x:xs) 

-- pattern data structure
data Pattern = WildcardPat | VariablePat (String) | UnitPat | ConstantPat (Int) | ConstructorPat (String, Pattern) | TuplePat ([Pattern]) deriving (Show)

-- value data structure
data Value = Constant (Int) | Unit | Constructor (String, Value) | Tuple [Value] deriving (Show)


g f1 f2 pat =
 let
   r = g f1 f2
 in
   case pat of
     WildcardPat -> f1 ()
     VariablePat x -> f2 x
     ConstructorPat (_, p) -> r p
     TuplePat values -> foldl (\i p -> (r p) + i) 0 values
     _ -> 0
     
-- g takes two functions (f1 and f2) and a value of type Pattern (pat).
-- f1 is a function that takes an empty tuple () and returns a number
-- f2 is a function that takes a string and returns a number
-- pat is a type of Pattern as described above
-- overall, g returns a number
  
-- countWildcards 
-- takes a Pattern and returns a number corresponding to the amount of WildcardPats in it

countWildcards pattern = g (\x -> 1) (\y -> 0) pattern

-- countWildAndVariableLengths
-- takes a Pattern
-- returns a number corresponding to the amount of Wildcard patterns + the sum of string lengths of all the variables in the variable patterns it contains

countWildAndVariableLengths pattern = g (\x -> 1) (\y -> length y) pattern

-- countAVar
-- takes a pair of a string and pattern
-- returns a number corresponding to the # of times the string appears in the pattern

countAVar (string, pattern) =  g (\x -> 0) (\y -> if y == string then 1 else 0) pattern

-- checkPat
-- takes a pattern
-- returns a bool -- true only if all the variables in pattern are distinct from each other (diff strings)

-- checkPat pattern = 
--   let
--     allStrings pattern = g (\x -> "") (\y -> y) pattern
--     let
--       
--   in

-- firstMatch
-- takes a Value and a list of Patterns
-- returns a Maybe [(String, Value)], Nothing if no Pattern in the list matches
-- or Just lst where lst is the list of bindings for the first pattern in the list that matches
-- use firstAnswer
       

                       