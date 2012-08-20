module SqlCompiler(
   compile
)where

import Parser
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad

type DefVal = Expr

type Context = [Map Name DefVal]

pop :: State Context ()  
pop = do
   c <- get
   put $ tail c
  
push :: State Context ()  
push = do
   c <- get
   put $ Map.empty:c

regDef :: Expr -> State Context ()
regDef (Def n d) = do
   t:r <- get
   put $ Map.insert n d t :r
refDef _ = return ()

regDefs :: [Expr] -> State Context ()
regDefs l =
   forM_ l $ \x -> do
      regDef x

getDef :: Name -> State Context (Maybe DefVal)
getDef n = do
   c <- get
   return $ msum $ map (Map.lookup n) c

compileM defs =
   regDefs defs

compile' (Program a b) = 
   Right $
      show $
         runState (compileM b) [Map.empty]

compile :: String -> String -> Either String String
compile fname input = 
   let
      sintaxTree = parseProg fname input
   in
      case (sintaxTree) of
         Left err -> Left $ "Parse error:\n" ++ show err 
         Right x  -> compile' x

      

