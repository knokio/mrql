module Main(
   main
)where

import SqlCompiler

main = do
   let fname = "examples/ex1.mrdb" 
   contents <- readFile fname
   case (getSql fname contents) of
      Left err -> do{ putStr "error at "
                    ; print err
                    }
      Right x  -> print x
