{-# LANGUAGE NoMonomorphismRestriction #-}

module Parser(
   parseProg
   ,ArgDeclarations
   ,Name
   ,Expr
   ,FunctionHeader
   ,Def
)where

import Text.Parsec hiding (State)
import Text.Parsec.Indent
import Control.Monad.State
import Text.Parsec.Expr

type IParser a = ParsecT String () (State SourcePos) a

iParse :: IParser a -> SourceName -> String -> Either ParseError a
iParse aParser source_name input =
  runIndent source_name $ runParserT aParser () source_name input

type ArgDeclarations = [String]
type Name = String


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
   deriving (Show)

data FunctionHeader = FunctionHeader Name ArgDeclarations
   deriving (Show)

data Def = DefScalar Name Expr
         | DefFn FunctionHeader [Expr]
         | DefQuery FunctionHeader [Def]
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
   return $ FunctionHeader i args

functionDHeader = do
   lit "def"
   functionHeader


functionDef = withBlock DefFn functionDHeader expr


scalarDef = do
   lit "def"
   i <- ident
   lit "="
   e <- expr
   return $ DefScalar i e

defQueryHead = do
   lit "def_query"
   functionHeader

defQuery = withBlock DefQuery defQueryHead def

gdefs :: IParser Def
gdefs = defQuery
    <|> def

prog :: IParser [Def]
prog = do
   commentsOrSpaces
   pdefs <- many gdefs
   eof
   return pdefs

parseProg fname input = iParse prog fname input

