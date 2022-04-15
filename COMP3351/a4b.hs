module A4b where
import JsonParser
import Data.Maybe

-- dot takes a json object and a string
-- if the json object contains a field given by second argument, it retuns Just v where v is the matching JSON value (otherwise Nothing)
-- returns a Maybe
dot json string =
  case json of
    ObjVal list -> let
                                searchList [] = Nothing
                                searchList (y:ys) = if (string == (fst y))
                                                              then Just (snd y)
                                                              else searchList ys
                              in
                                searchList list
    _ -> error "Not an ObjVal"

-- filterKeys
-- takes a function (predicate) and applies it to each (key,value) pair in the json object
-- if predicate is True, the pair is kept, otherwise it is ignored
-- returns JSON object only containing those True keys
-- throws error if not an ObjVal

filterKeys predicate json = 
  case json of
    ObjVal list -> ObjVal (filter (\(string, val) -> predicate string) list)
    _ -> error "Not ObjVal"

-- keyCount
-- takes a JSON object and returns number of keys in JSON object
-- throws error if not an ObjVal

keyCount json = 
  case json of
    ObjVal x -> length x
    _ -> error "Not ObjVal"
    
-- keyList
-- takes JSON object and returns list of all keys in JSON object
-- throws error if not an ObjVal

keyList json = 
  case json of
    ObjVal (x:xs) -> (fst x) : keyList (ObjVal xs)
    ObjVal [] -> []
    _ -> error "Not ObjVal"

-- arrayLength
-- takes JSON array and returns the number of elements in the array
-- throws error if not an Array

arrayLength jsonArray =
  case jsonArray of
    Array x -> length x
    _ -> error "Not Array"
    
-- filterRange
-- takes a low, high ints and json object and returns a json array containing the range of elements 
-- from low to high
-- throws error if not an Array
-- ex. fr 2 3  (Array [NumVal 0, NumVal 1, NumVal 2, NumVal 3, NumVal 4]) = [NumVal 2.0, NumVal 3.0]

filterRange low high jsonArray = 
  case jsonArray of
    Array x -> Array (take (high - (low - 1)) (drop low x))
    _ -> error "Not Array"

-- filterArray
-- takes a predicate and a json array
-- returns json array containing only those elements where predicate is true
-- throws error if not an Array

filterArray predicate jsonArray =
  case jsonArray of
    Array x -> Array (filter predicate x)       
    _ -> error "Not Array"

-- extractElements
-- takes JSON array, list of ints (indices) into the array
-- returns new array consisting of only those indices
-- throws error if not an Array

extractElements json indices = 
  case json of
    Array x -> let
                       extractElement [] [] = []
                       extractElement _ [] = []
                       extractElement [] _ = []
                       extractElement arr (i:is) = (arr !! i) : extractElement arr is
                     in
                       Array (extractElement x indices)
    _ -> error "Not Array"

-- increasingIncidents
-- processes json file to find all diseases that have had increasing numbers of incidents 2013 to 2017
-- takes ObjVal and returns string?

increasingIncidents objval = 
  let
    values = dot objval "data"
  in
    let
      importantValues = 
        case values of
          Just x -> case x of
                           Array y -> Array (map (\list -> extractElements list [8, 17, 25]) y)
          _ -> error "not Maybe x"
    in
      case importantValues of -- is array of arrays []
        Array list -> let -- list is an array
                             helperFun acc [] = acc
                             helperFun acc (x:xs) =
                               case x of
                                 Array (StrVal x: StrVal y: StrVal z: []) -> if y > z 
                                                                                               then helperFun (acc ++ ((show x) ++ ": " ++ (show z) ++ " cases in 2013, " ++ (show y) ++ " cases in 2017\n")) xs
                                                                                               else (helperFun acc xs)
                                 Array (StrVal x: NullVal : NullVal: []) -> helperFun acc xs
                                 Array (StrVal x: StrVal y: NullVal: []) -> helperFun (acc ++ ((show x) ++ ": 0 cases in 2013, " ++ (show y) ++ " cases in 2017\n")) xs
                                 Array (StrVal x: NullVal : StrVal z: []) -> helperFun acc xs
                          --_ -> helperFun acc xs
                           in
                             helperFun "" list
                             
-- createArrayOfObjVal listOfArrays = -- [Array [StrVal x, y, z], Array [StrVal x, y, z]]
--   case listOfArrays of -- x would return Array [StrVal x, y, z]
--     (x:xs) -> 

  



  
 
