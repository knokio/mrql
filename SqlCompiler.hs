{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module SqlCompiler(
   compile
)where

import Parser
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad
import qualified Data.String.Utils as Str
import Control.Monad.Error

type Compiled = [String] -> String

type Context = [Map Name Compiled]

newtype Compiler a = C {
      runC :: ErrorT String (State Context) a
    } deriving (Monad, MonadError String, MonadState Context)


tables = ["stuff"]

pop :: Compiler ()
pop = do
   c <- get
   let res = tail c
   if null res then
      throwError " pop\nInternal error: Can't pop all contexts"
   else
      put $ tail c
  
push :: Compiler ()  
push = do
   c <- get
   put $ Map.empty:c

regCompiled :: Name -> Compiled -> Compiler ()
regCompiled n v = do
   t:r <- get
   put $ Map.insert n v t :r

regDef :: Expr -> Compiler ()
regDef (Def n d) = do
   ebld <- buildExpr d
   regCompiled n $ ebld


regDefs :: [Expr] -> Compiler ()
regDefs l =
   forM_ l $ \x -> do
      regDef x

getDef :: Name -> Compiler Compiled
getDef n = do
   c <- get
   case msum $ map (Map.lookup n) c of
      Nothing -> throwError ("No Def for "++ n)
      Just v -> return v

buildExpr :: Expr -> Compiler Compiled
buildExpr (Identifier id) = do
   getDef id
buildExpr (Projection a (Identifier b)) = do
   as <- buildExpr a
   return (\x -> as x ++ "." ++ b)
buildExpr (IntExpr i) = return (\x -> show i)
buildExpr d@(Def n e) = do
   regDef d
   getDef n
buildExpr fn@(Fn args exps) = do
   push
   fnProcessInitExprs fn
   res <- buildExpr $ last exps
   pop
   return res
buildExpr (Query args defs) = do
   push
   forM_ (zip [0..] args) $ \(idx, arg) -> do
      regCompiled arg (\x -> x!!idx)
   regDefs defs
   res <- buildQuery defs
   pop
   return res
buildExpr (Object fields) = do
   fs <- forM fields $ \(StrExpr n,v) -> do 
      value <- buildExpr v
      return $ (\x -> value x ++ " as " ++ n)
   return $ (\x -> Str.join ", " (map (\y -> y x) fs))
buildExpr e = do
   return (\x -> show e)


fnProcessInitExprs :: Expr -> Compiler ()
fnProcessInitExprs (Fn args exps) = do
   forM_ (zip [0..] args) $ \(idx, arg) -> do
      regCompiled arg (\x -> x!!idx)
   forM_ (init exps) $ \e -> do
      buildExpr e


getDefFromList :: Name -> [Expr] -> Expr
getDefFromList n le  = 
   head $ [d| Def nm d <- le, nm == n]

buildReduceSelect :: Expr -> Compiler Compiled
buildReduceSelect fn@(Fn args exps)  = do
   push
   fnProcessInitExprs fn
   let Object resObj = (last exps)
   lres <- mapM (procBaseReduce args ) resObj
   pop
   return (\x ->  Str.join ", " ( map (\y-> y x) lres ))

procBaseReduce :: [Name] -> (Expr, Expr) -> Compiler Compiled
procBaseReduce args (StrExpr n, e) = do
   exp <- procBaseReduceE args e
   return (\x -> exp x ++ " as " ++ n)

procBaseReduceE args p@(Plus p1 p2) = do
   inner <- getReducePreFn args p1 p2
   return (\x -> "sum("++ inner x ++")")


brotherExprs (v1,v2) (e1,e2) =
   if not $ findExpr v1 e1 then
      if not $ findExpr v2 e2 then
        replaceExpr v2 (Identifier "#") e1 == replaceExpr v1 (Identifier "#") e2
      else 
         False
   else
      if not (findExpr v2 e1) && not (findExpr v1 e2) then
        replaceExpr v2 (Identifier "#") e2 == replaceExpr v1 (Identifier "#") e1
      else
         False
         
getReducePreFn :: [Name] -> Expr -> Expr -> Compiler Compiled
getReducePreFn [a1,a2] e1 e2 =
   let
      i1 = Identifier a1
      i2 = Identifier a2
      is = Identifier "#"
   in
      if brotherExprs (i1, i2) (e1,e2) then
         do
            push
            regCompiled "#" (\x->x!!0)
            exp <- buildExpr $ replaceExpr i1 is (replaceExpr i2 is e1)
            pop
            return exp  
      else
         throwError "Can't calculate reduce"


buildQuery :: [Expr] -> Compiler Compiled
buildQuery defs = do
   push

   sourceTable <- getDef "table"
   map <- getDef "map"
   let map_select = map ["a"]
   let res_map = "select " ++ map_select ++" from " ++ sourceTable [] ++ " a"
   
   reduce_select <- buildReduceSelect $ getDefFromList "reduce" defs
   let res = "select " ++ reduce_select ["b"] ++ " from (" ++ res_map ++ ") b"

   pop
   return (\x -> res) 


sqlCompiler :: Program -> Compiler String
sqlCompiler (Program exports defs) = do
   forM_ tables $ \tb -> do
      regCompiled tb (\x -> tb )
   regDefs defs
   ds <- mapM getDef exports
   return $ Str.join "\n\n" (map (\x -> x []) ds)


runCompiler :: (Program -> Compiler String) -> Program -> Either String String
runCompiler c p = 
   case runState (runErrorT (runC (c p))) [Map.empty] of
      (Left err, _) -> Left err
      (Right res, _ ) -> Right res


compile :: String -> String -> Either String String
compile fname input = 
   let
      sintaxTree = parseProg fname input
   in
      case (sintaxTree) of
         Left err -> Left $ "Parse error:\n" ++ show err 
         Right x  -> runCompiler sqlCompiler x
         --Right x -> Right $ show sintaxTree


