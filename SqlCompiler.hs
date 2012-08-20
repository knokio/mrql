module SqlCompiler(
   getSql
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

regDef :: (Name, DefVal) -> State Context ()
regDef (n,d) = do
   t:r <- get
   put $ Map.insert n d t :r

regListDefs :: [(Name, DefVal)] -> State Context ()
regListDefs l = do
   t:r <- get
   put $ (t `Map.union` (Map.fromList l)):r
  

getDef :: Name -> State Context (Maybe DefVal)
getDef n = do
   c <- get
   return $ msum $ map (Map.lookup n) c


--compile [DefQuery _ defs] = 

getSql fname input = parseProg fname input

