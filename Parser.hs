{-# LANGUAGE NoMonomorphismRestriction #-}

module Parser(
   parseProg
   ,Name(..)
   ,Expr(..)
   ,Program(..)
   ,replaceExpr
   ,findExpr
)where

import Text.Parsec hiding (State)
import Text.Parsec.Indent
import Control.Monad.State
import Text.Parsec.Expr

type IParser a = ParsecT String () (State SourcePos) a

iParse :: IParser a -> SourceName -> String -> Either ParseError a
iParse aParser source_name input =
  runIndent source_name $ runParserT aParser () source_name input

type Name = String
type Param = String

data Program = Program [String] [Expr]
   deriving (Show)

data Expr = Identifier Name
          | Object [(Expr,Expr)]
          | Neg Expr
          | Plus Expr Expr
          | Subtract Expr Expr
          | Mult Expr Expr
          | Div Expr Expr
          | StrExpr String
          | IntExpr Integer
          | Projection Expr Expr
          | Fn [Param] [Expr]
          | Query [Param] [Expr]
          | Def Name Expr
   deriving (Show, Eq)


findExpr :: Expr -> Expr -> Bool
findExpr find expr | expr == find = True 
findExpr f (Object l) = or $ map (\(x,y) -> findExpr f x || findExpr f y) l
findExpr f (Neg e) = findExpr f e
findExpr f (Plus a b) =  findExpr f a || findExpr f b
findExpr f (Subtract a b) = findExpr f a || findExpr f b
findExpr f (Mult a b) = findExpr f a || findExpr f b
findExpr f (Div a b) = findExpr f a || findExpr f b
findExpr f (Projection a b) = findExpr f a || findExpr f b
findExpr f (Fn a b) = or $(map  (findExpr f) b)
findExpr f (Query a b) = and $ (map  (findExpr f) b)
findExpr f (Def a b) = findExpr f b
findExpr f e = False



replaceExpr :: Expr -> Expr -> Expr -> Expr
replaceExpr find replace expr | expr == find = replace 
replaceExpr f r (Object l) = Object $ map (\(x,y) -> (replaceExpr f r x, replaceExpr f r y)) l
replaceExpr f r (Neg e) = Neg $replaceExpr f r e
replaceExpr f r (Plus a b) = Plus (replaceExpr f r a) (replaceExpr f r b)
replaceExpr f r (Subtract a b) = Subtract (replaceExpr f r a) (replaceExpr f r b)
replaceExpr f r (Mult a b) = Mult (replaceExpr f r a) (replaceExpr f r b)
replaceExpr f r (Div a b) = Div (replaceExpr f r a) (replaceExpr f r b)
replaceExpr f r (Projection a b) = Projection (replaceExpr f r a) (replaceExpr f r b)
replaceExpr f r (Fn a b) = Fn  a  (map  (replaceExpr f r) b)
replaceExpr f r (Query a b) = Query  a  (map  (replaceExpr f r) b)
replaceExpr f r (Def a b) = Def a (replaceExpr f r b)
replaceExpr f r e = e

lineComment = do
   string "--"
   skipMany $ noneOf "\n\r"
   newline

blockComment = do
   string "{-"
   manyTill anyChar (string "-}") 
   return ' '

commentOrSpace =   try lineComment
               <|> try blockComment
               <|> space

commentsOrSpaces = skipMany commentOrSpace

tk :: IParser a -> IParser a
tk a = do
   v <- a
   commentsOrSpaces
   return v

quotedString = tk $
    do char '"'
       content <- many quotedChar
       char '"' <?> "quote at end of cell"
       return  content

quotedChar =
        noneOf "\"\\"
    <|> try (string "\\\"" >> return '"')

integer = tk $ do
   d <- many1 digit
   return $ IntExpr $ read d
   

ident :: IParser String
ident = tk $ do
   h <- letter
   r <- many $ alphaNum <|> oneOf "_"
   return $ h:r

lit :: String -> IParser String
lit l = tk $ string l

objPair = do
   key <-expr
   lit ":"
   value <- expr
   return (key,value)

obj = do
   lit "{"
   o <- sepBy objPair  (lit ",")
   lit "}"
   return $ Object o

identExpr = do
   i <- ident
   return $ Identifier i

parensExpr :: IParser Expr
parensExpr = do
   lit "("
   e <- expr
   lit ")"
   return e

expr    = buildExpressionParser table term
        <?> "expression"

quotedStringExpr = liftM StrExpr quotedString

term    =   obj
        <|> parensExpr
        <|> try scalarDef
        <|> identExpr
        <|> quotedStringExpr
        <|> integer
        <?> "simple expression"

table   = [ [Infix (do{ char '.'; return Projection }) AssocLeft ]
          , [prefix "-" Neg, prefix "+" id ]
          , [binary "*" Mult AssocLeft, binary "/" Div AssocLeft ]
          , [binary "+" Plus AssocLeft, binary "-" Subtract AssocLeft ]
          ]
        
binary  name fun assoc = Infix (do{ lit name; return fun }) assoc
prefix  name fun       = Prefix (do{ lit name; return fun })
postfix name fun       = Postfix (do{ lit name; return fun })



argDef = ident

def = try scalarDef
  <|> functionDef
  <?> "Def"


functionHeader = do
   i <- ident
   lit "("
   args<-sepBy argDef (char ',')
   lit ")"
   lit ":"
   return $ (i, args)

functionDHeader = do
   lit "def"
   functionHeader


functionDef' = withBlock (,) functionDHeader expr

functionDef = do
   ((n,p),es) <- functionDef'
   return $ Def n (Fn p es)

scalarDef = do
   lit "def"
   i <- ident
   lit "="
   e <- expr
   return $ Def i e

defQueryHead = do
   lit "def_query"
   functionHeader

defQuery' = withBlock (,) defQueryHead def

defQuery = do
   ((n,p), d) <- defQuery'
   return $ Def n (Query p d)

exports = do
   lit "export"
   sepBy ident (lit ",")

gdefs :: IParser Expr
gdefs = try defQuery
    <|> def
    <?> "Global def"

prog :: IParser Program
prog = do
   commentsOrSpaces
   e <- exports
   pdefs <- many gdefs
   eof
   return $ Program e pdefs

parseProg fname input = iParse prog fname input

