-- Put your Parser implmenetation in this file.
module ParserImpl where

import Types
import           Data.Char
import           Text.ParserCombinators.ReadP
import           Control.Applicative            ( (<|>) )
import           Data.List

parseString :: String -> Either ErrMsg Program
parseString s = case readP_to_S stmts s of
  [] -> Left "no matches"
  x  -> case last x of
    (p, "") -> Right p
    (_, e ) -> Left e



type Parser a = ReadP a   -- may use synomym for easier portability to Parsec

keyword x = x `elem` ["false", "if", "implies", "in", "is", "not", "true", "unless"]


nStart = satisfy $ \x -> x `elem` (['a' .. 'z'] ++ ['A' .. 'Z'])

chars = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['_'] ++ ['0' .. '9']

nChar = satisfy $ \x -> x `elem` chars


name = do
  c  <- nStart
  cs <- many nChar

  let id = c : cs
  if not $ isReserved id then return id else pfail

program :: ReadP Program
program = lexeme $ do
  rule `sepBy1` symb '.'

rule :: ReadP Rule
rule =
  lexeme
    $   do

          Atom 
    <|> do
          SExp <$> expr






symb s = lexeme $ do
  satisfy (== s)

word s = lexeme $ do
  string s
identExpr = lexeme $ do
  Var <$> ident

ident = req $ do
  c  <- idStart
  cs <- many idChar

  let id = c : cs
  if not $ isReserved id then return id else pfail

idStart = satisfy $ \x -> x `elem` (['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['_'])

chars = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['_'] ++ ['0' .. '9']

idChar = satisfy $ \x -> x `elem` chars

isReserved x = x `elem` ["None", "True", "False", "not", "in", "for", "if"]

expr = expr' <|> notExpr

expr' = expr'' <|> relExpr

expr'' = term `chainl1` addOp

term = factor `chainl1` multOp

factor =
  numExpr
    <|> identExpr
    <|> keywordConst
    <|> stringConst
    <|> parenthesisExpr
    <|> callExpr
    <|> listExpr
    <|> comprExpr

notExpr = req $ do
  string "not"
  s <- look
  case s of
    x : xs | x `elem` chars -> pfail
    _                       -> do
      skipSpaces
      Not <$> expr

relExpr =
  lexeme
    $   do
          left <- expr''
          op   <- relOp
          Oper op left <$> expr''
    <|> do
          left <- expr''
          op   <- invRelOp
          Not . Oper op left <$> expr''

relOp =
  lexeme
    $   do
          symb '<'
          return Less
    <|> do
          symb '>'
          return Greater
    <|> do
          word "=="
          return Eq
    <|> do
          string "in"
          end
          return In

invRelOp =
  lexeme
    $   do
          word "<="
          return Greater
    <|> do
          word ">="
          return Less
    <|> do
          word "!="
          return Eq
    <|> do
          string "not"
          skipMany1 whitespace
          string "in"
          return In

numExpr = Const <$> numConst

numConst =
  lexeme
    $   do
          sign <- option "" (string "-")
          i    <- nonZero
          n    <- many digit
          return $ IntVal (read (sign ++ i : n))
    <|> do
          sign <- option "" (string "-")
          zero <- string "0"
          return $ IntVal (read (sign ++ zero))

nonZero = satisfy $ \x -> x `elem` ['1' .. '9']

digit = satisfy $ \x -> x `elem` ['0' .. '9']

keywordConst =
  req
    $   do
          string "True"
          return $ Const TrueVal
    <|> do
          string "False"
          return $ Const FalseVal
    <|> do
          string "None"
          return $ Const NoneVal

stringConst = lexeme $ do
  satisfy (== '\'')
  n <- many stringChars
  satisfy (== '\'')
  return $ Const (StringVal (intercalate "" n))

stringChars = escaped <|> ascii

escaped =
  do
      string "\\\'"
      return "\'"
    <|> do
          satisfy (== '\\')
          satisfy (== '\\')
          return "\\"
    <|> do
          string "\\n"
          return "\n"
    <|> do
          string "\\\n"
          return ""

ascii = do
  s <- satisfy (\x -> x `elem` ([' ' .. '~'] \\ ['\\', '\'']))
  return [s]

addOp =
  lexeme
    $   do
          symb '+'
          return $ Oper Plus
    <|> do
          symb '-'
          return $ Oper Minus

multOp =
  lexeme
    $   do
          symb '*'
          return $ Oper Times
    <|> do
          string "//"
          return $ Oper Div
    <|> do
          symb '%'
          return $ Oper Mod

parenthesisExpr = lexeme $ do
  symb '('
  e <- expr
  symb ')'
  return e

notIf = lexeme $ do
  string "if"
  CCIf <$> expr

ended p = do
  a <- p
  end
  return a

lexeme :: ReadP b -> ReadP b
lexeme p = do
  a <- p
  skipSpaces
  comment
  return a

comment = optional $ between
  (symb '#')
  (do
    satisfy (== '\n')
    return () <|> eof
  )
  (munch (/= '\n'))

callExpr = lexeme $ do
  id <- ident
  symb '('
  e <- exprz
  symb ')'
  return $ Call id e

listExpr = lexeme $ do
  symb '['
  e <- exprz
  symb ']'
  return $ List e

exprz = lexeme $ do
  option [] exprs

exprs = lexeme $ do
  expr `sepBy1` symb ','

comprExpr = lexeme $ do
  symb '['
  e <- expr
  f <- forClause
  c <- clausez
  symb ']'
  return $ Compr e (f : c)

clausez = lexeme $ do
  many $ do
    forClause <|> ifClause

forClause = lexeme $ do
  word "for"
  id <- ident
  word "in"
  e <- expr
  return $ CCFor id e

ifClause = lexeme $ do
  word "if"
  e <- expr
  return $ CCIf e

end = skipMany1 whitespace <|> eof

req p = do
  a <- p
  s <- look
  case s of
    x : xs | x `elem` chars -> pfail
    _                       -> do
      skipSpaces
      return a

whitespace = satisfy $ \x -> x `elem` [' ', '\t', '\n']
