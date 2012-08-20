{-# LANGUAGE NoMonomorphismRestriction #-}

module Parser(
   parseProg
   ,Name(..)
   ,Expr(..)
   ,Program(..)
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
          | Seq [Expr]
          | Fn [Param] Expr
          | Def Name Expr
   deriving (Show)


lineComment = do
   string "--"
   skipMany $ noneOf "\n\r"
   newline

commentOrSpace =   lineComment
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

term    =  parensExpr 
        <|> identExpr
        <|> obj
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
   return $ Def n (Fn p (Seq es))

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
   return $ Def n (Fn p (Seq d))

exports = do
   lit "export"
   sepBy ident (lit ",")

gdefs :: IParser Expr
gdefs = defQuery
    <|> def

prog :: IParser Program
prog = do
   commentsOrSpaces
   e <- exports
   pdefs <- many gdefs
   eof
   return $ Program e pdefs

parseProg fname input = iParse prog fname input

