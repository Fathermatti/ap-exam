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

keyword x =
  x `elem` ["false", "if", "implies", "in", "is", "not", "true", "unless"]

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
  a <- atom
  c <- ruleRule
  return $ Rule a c

ruleRule = do
  word "if"
  cond
  <++ do
          word "unless"
          c <- cond 
          return $ CNot c
  <++ do
          return CTrue

atom = do
  p <- name
  symb '('
  t <- termz
  symb ')'
  return $ Atom p t

word s = lexeme $ do
  string s

cond = cond' `chainr1` impliesCond

impliesCond = lexeme $ do
  word "implies"
  return $ COr . CNot

cond' = do
  cond'' `chainl1` orCond

orCond = lexeme $ do
  word "or"
  return COr

cond'' = do
  cond''' `chainl1` andCond

andCond :: ReadP (Cond -> Cond -> Cond)
andCond = lexeme $ do
  word "and"
  return CAnd

cond''' =
  do
      word "not"
      c <- cond'''
      return $ CNot c
    <|> cond''''

cond'''' =
  do
      a <- atom
      return $ CAtom a
    <|> do
          t1 <- term
          word "is"
          t2 <- term
          return $ CEq t1 t2
    <|> do
          t1 <- term
          word "is"
          word "not"
          t2 <- term
          return $ CNot (CEq t1 t2)
    <|> do
          word "true"
          return CTrue
    <|> do
          word "false"
          return $ CNot CTrue
    <|> do
          symb '('
          c <- cond
          symb ')'
          return c

name = lexeme $ do
  c  <- nStart
  cs <- many nChar
  let id = c : cs
  if not $ keyword id then return id else pfail

termz =
  do
      terms
    <++ do
          return []

terms = lexeme $ do
  term `sepBy1` symb ','

term =
  do
      vname
    <|> constant

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
