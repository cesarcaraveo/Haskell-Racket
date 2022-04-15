import A4b
import System.IO
import System.Environment
import JsonParser
import JsonLexer
import Control.Monad

main =
  do
    args <- getArgs -- get the arguments passed to our main function
  -- if the length of the number of arguments is > 0, then assign the first thing
  -- as the name of the file to the variable name filename
    when (length args > 0) $ do
      let filename = (head args)
      obj <- parse filename -- now call your function parse using this filename
      --print (dot obj "data")
      putStrLn (increasingIncidents obj)
      
  
parse fName = do
  handle <- openFile fName ReadMode -- open the file for reading
  let pos = initPos fName 1 0 -- initialize a position object that the lexer and parser need
  parseObject (makeTokenizer handle) pos