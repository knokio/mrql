module Main(
   main
)where

import SqlCompiler

main = do
   let fname = "examples/ex1.mrdb" 
   contents <- readFile fname
   case (compile fname contents) of
      Left err -> do{ putStr "error at "
                    ; putStrLn err
                    }
      Right x  -> putStrLn x
