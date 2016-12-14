module False.Parser ( parseFalse
                    , FalseToken (..)
                    , FalseParser
                    ) where

import Data.Int

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String


{- TODO:
   * global variables
-}


-- | Tokens of the Flase Language
data FalseToken i = Add | Sub | Mul | Div
                  | Neg
                  | Eq | Gt
   {- bitwise -}  | And | Or | Not
                  | IntLit i
                  | CharLit Char
                  | PutVar Char | GetVar Char
                  | Lambda [FalseToken i] | Apply
                  | Dup | Drop | Swap | Rot | Pick
                  | When | While
                  | Print String | PrintInt | Flush
                  | Putch | Getch 
                  deriving Show

type FalseParser i = Parser [FalseToken i]  

genSimpleParser :: Char -> FalseToken i -> Parser (FalseToken i)
genSimpleParser ch tok = char ch >> return tok

parseAdd, parseSub, parseMul, parseDiv, parseNeg, parseEq, parseGt
  , parseAnd, parseOr, parseNot, parseApply, parseDup, parseDrop, parseSwap
  , parseRot, parsePick, parseWhen, parseWhile, parsePrintInt, parseFlush
  , parsePutch, parseGetch, parsePutVar, parseGetVar :: Parser (FalseToken i)

parseAdd = genSimpleParser '+' Add
parseSub = genSimpleParser '-' Sub
parseMul = genSimpleParser '*' Mul
parseDiv = genSimpleParser '/' Div
parseNeg = genSimpleParser '_' Neg
parseEq  = genSimpleParser '=' Eq
parseGt  = genSimpleParser '>' Gt
parseAnd = genSimpleParser '&' And
parseOr  = genSimpleParser '|' Or
parseNot = genSimpleParser '~' Not
parseApply = genSimpleParser '!' Apply
parseDup   = genSimpleParser '$' Dup
parseDrop  = genSimpleParser '%' Drop
parseSwap  = genSimpleParser '\\' Swap
parseRot   = genSimpleParser '@' Rot
parsePick  = genSimpleParser '`' Pick
             <|> genSimpleParser 'ø' Pick -- NOTE The standard defines pick as ø
parseWhen  = genSimpleParser '?' When
parseWhile = genSimpleParser '#' While
parsePrintInt = genSimpleParser '.' PrintInt
parseFlush    = genSimpleParser 'ß' Flush   -- TODO reassign
parsePutch    = genSimpleParser ',' Putch
parseGetch    = genSimpleParser '^' Getch



parsePutVar   = do
  var <- lower
  genSimpleParser ':' $ PutVar var
parseGetVar   = do
  var <- lower
  genSimpleParser ';' $ GetVar var



parseIntLit :: (Read i) => Parser (FalseToken i)
parseIntLit = do
  nums <- many1 digit
  return $ IntLit (read nums)



parseCharLit :: Parser (FalseToken i)
parseCharLit = do
  char '\''
  ch <- anyChar
  return $ CharLit ch



-- parseVarRef :: Parser (FalseToken i)
-- parseVarRef = lower >>= (return . VarRef)



parseLambda :: (Read i) => Parser (FalseToken i)
parseLambda = do
  char '['
  expr <- parseFalse
  char ']'
  return $ Lambda expr



parsePrintString :: Parser (FalseToken i)
parsePrintString = do
  char '"'
  str <- many $ noneOf "\""
  char '"'
  return $ Print str


parseToken :: (Read i) => Parser (FalseToken i)
parseToken =
  parseAdd
  <|> parseSub
  <|> parseMul
  <|> parseDiv
  <|> parseNeg
  <|> parseEq
  <|> parseGt
  <|> parseAnd
  <|> parseOr
  <|> parseNot
  <|> parseApply
  <|> parseDup
  <|> parseDrop
  <|> parseSwap
  <|> parseRot
  <|> parsePick
  <|> parseWhen
  <|> parseWhile
  <|> parsePrintInt
  <|> parseFlush
  <|> parsePutch
  <|> parseGetch
  <|> try parsePutVar
  <|> try parseGetVar
  <|> parseIntLit
  <|> parseCharLit
--  <|> parseVarRef
  <|> parseLambda
  <|> parsePrintString


parseComment, parseSpace :: Parser ()
parseComment = do
  char '{'
  many $ noneOf "}"
  char '}'
  return ()
  
parseSpace  = space >> return ()


parseFnord :: Parser ()
parseFnord = do
  skipMany $ parseComment <|> parseSpace


parseFalseToken :: (Read i) => Parser (FalseToken i)
parseFalseToken = do
  parseFnord
  tok <- parseToken
  parseFnord
  return tok
  

parseFalse :: (Read i) => FalseParser i
-- FIXME it should be possible to write an empty program
parseFalse = many1 parseFalseToken


