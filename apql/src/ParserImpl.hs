-- Put your Parser implmenetation in this file.
module ParserImpl where

import           Types
import           Data.Char
import           Text.ParserCombinators.ReadP
import           Control.Applicative            ( (<|>) )
import           Data.List

parseString :: String -> Either ErrMsg Program
parseString s = case readP_to_S program s of
  [] -> Left $ EUser "hey"
  x  -> case last x of
    (p, "") -> Right p
    (_, e ) -> Left $ EUser e

type Parser a = ReadP a   -- may use synomym for easier portability to Parsec

keyword :: [Char] -> Bool
keyword x =
  x `elem` ["false", "if", "implies", "in", "is", "not", "true", "unless"]

nStart :: ReadP Char
nStart = satisfy $ \x -> x `elem` (['a' .. 'z'] ++ ['A' .. 'Z'])

chars = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['_'] ++ ['0' .. '9']

nChar = satisfy $ \x -> x `elem` chars

program :: ReadP Program
program = do
  skipSpaces
  r <- rule `sepBy1` symb '.'
  skipSpaces
  eof
  return r

rule = do
  p <- name
  symb '('
  t <- termz
  symb ')'
  return $ Rule (Atom p t) CTrue

name = lexeme $ do
  c  <- nStart
  cs <- many nChar

  let id = c : cs
  if not $ keyword id then return id else pfail

termz =
  do
      terms
    <|> do
          return []

terms = lexeme $ do
  term `sepBy1` symb ','

term = do
       vname
    <|> 
        constant

vname = lexeme $ do 
    TVar <$> name

constant = lexeme $ do
          symb '"'
          s <- many nChar
          symb '"'
          return $ TData s

symb :: Char -> ReadP Char
symb s = lexeme $ do
  satisfy (== s)

lexeme p = do
  a <- p
  skipSpaces
  return a
