module Main(
   main
)where

import Parser

main = do
   let fname = "examples/ex1.mrdb" 
   contents <- readFile fname
   case (parseProg fname contents) of
      Left err -> do{ putStr "parse error at "
                    ; print err
                    }
      Right x  -> print x
