{-# LANGUAGE NoMonomorphismRestriction #-}

module Parser(
   parseProg
)where

import Text.Parsec hiding (State)
import Text.Parsec.Indent
import Control.Monad.State

type IParser a = ParsecT String () (State SourcePos) a

iParse :: IParser a -> SourceName -> String -> Either ParseError a
iParse aParser source_name input =
  runIndent source_name $ runParserT aParser () source_name input

type ArgDeclarations = [String]
type Name = String

data FunctionHeader = FunctionHeader Name ArgDeclarations
   deriving (Show)

data DefQuery = DefQuery FunctionHeader [DefQueryProp]
   deriving (Show)

data DefQueryProp = DefQPropTable Name
                  | DefQPropMap String
   deriving (Show)

lineComment = do
   string "--"
   skipMany $ noneOf "\n\r"
   newline

commentOrSpace =   lineComment
               <|> space

commentsOrSpaces = skipMany commentOrSpace

ident = do
   i<-many $ alphaNum <|> oneOf "_"
   commentsOrSpaces
   return i

lit l= do
   string l
   commentsOrSpaces


argDef = ident

defQueryBlockElement =   try tableDef
                     <|> mapDef

tableDef = do
   lit "table"
   lit "="
   i <- ident
   return $ DefQPropTable i

cenas _ _ = DefQPropMap "olare"

mapDef = withBlock cenas (lit "map") (lit "ola")

defQueryHead = do
   lit "def_query"
   i <- ident
   lit "("
   args<-sepBy argDef (char ',')
   lit ")"
   lit ":"
   return $ FunctionHeader i args

defQuery = withBlock DefQuery defQueryHead defQueryBlockElement

defs = defQuery

prog = do
   commentsOrSpaces
   defs <- many defs
   eof
   return defs

parseProg fname input = iParse prog fname input

