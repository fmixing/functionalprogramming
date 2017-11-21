{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExplicitForAll            #-}
{-# LANGUAGE ExplicitNamespaces        #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Lib3
       (
         evaluate
       , Expr (..)
       , Environment (..)
       , ArithmeticError (..)
       , aExpr
       , whileParser
       , AExpr (..)
       , ABinOp (..)
       ) where

import qualified Data.Map.Strict as Map (Map, fromList, insert, lookup)
import Control.Applicative (liftA2)
import Control.Monad.Reader (Reader, asks, runReader)
import Control.Monad (fail)
import Data.Maybe (maybe)

import Data.Void (Void)
import Text.Megaparsec (Parsec, notFollowedBy, between, try, many, (<|>), parseTest', parseTest, eof, parse)
import Text.Megaparsec.Char (space, space1, char, string, alphaNumChar, letterChar)
import Text.Megaparsec.Expr (Operator (..), makeExprParser)
import qualified Text.Megaparsec.Char.Lexer as L



type VarName = String
type Const = Integer
data Expr = Var VarName | Lit Const | Add Expr Expr | 
            Mul Expr Expr | Sub Expr Expr | Div Expr Expr | 
            Let VarName Expr Expr deriving (Show, Eq)

data Environment = Environment { dict  :: Map.Map VarName Const }

data ArithmeticError = DivisionByZero | NameNotFound deriving (Show, Eq)

getVarValue :: VarName -> Reader Environment (Either ArithmeticError Const)
getVarValue varName = asks ((maybe (Left NameNotFound) (Right . id)) . Map.lookup varName . dict)

evaluateInternal :: Expr -> Expr -> (Const -> Const -> Const) -> Reader Environment (Either ArithmeticError Const)
evaluateInternal x y func = asks (\env -> 
    let resx = runReader (evaluate x) env
        resy = runReader (evaluate y) env
    in liftA2 func resx resy)

evaluate :: Expr -> Reader Environment (Either ArithmeticError Integer)
evaluate (Lit x) = return $ Right x
evaluate (Var name) = getVarValue name
evaluate (Add x y) = evaluateInternal x y (+)
evaluate (Mul x y) = evaluateInternal x y (*)
evaluate (Sub x y) = evaluateInternal x y (-)
evaluate (Div x y) = asks (\env -> let resy = runReader (evaluate y) env in 
    resy >>= (\res -> if res == 0 then Left DivisionByZero else liftA2 (div) (runReader (evaluate x) env) resy))
evaluate (Let name y z) = asks (\env -> let resy = runReader (evaluate y) env in 
    resy >>= (\res -> runReader (evaluate z) (Environment $ Map.insert name res $ dict env)))



-- x + 3 * (let x = 2 in x / 2)

data AExpr = AVar VarName
           | ALit Const
           | Neg AExpr
           | ABinary ABinOp AExpr AExpr
           | ALet VarName AExpr AExpr
           deriving (Show, Eq)

data ABinOp = AAdd
            | ASub
            | AMul
            | ADiv
            deriving (Show, Eq)


-- data ParsecT e s m a
-- e - error, s - stream type, m - underlying monad, a - return type
-- type Parsec e s = ParsecT e s Identity - non-transformer variant of the ParsecT - как Reader от ReaderT
type Parser = Parsec Void String -- e - ошибка - Void, s - stream type - String

whileParser :: Parser AExpr
whileParser = between space eof aExpr

aExpr :: Parser AExpr
aExpr = makeExprParser aTerm aOperators

aTerm :: Parser AExpr
aTerm = parens letParser
    <|> AVar <$> varNameParserS
    <|> ALit <$> integer
    <|> parens aExpr

aOperators :: [[Operator Parser AExpr]]
aOperators = 
    [[ Prefix  (Neg <$ symbol "-"), 
       InfixL  ((ABinary AMul) <$ symbol "*"),
       InfixL  ((ABinary ADiv) <$ symbol "/")],
     [ InfixL  ((ABinary AAdd) <$ symbol "+"),
       InfixL  ((ABinary ASub) <$ symbol "-")]]

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: String -> Parser String
symbol = L.symbol space

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

integer :: Parser Integer
integer = lexeme L.decimal

varNameParserS :: Parser String
varNameParserS = (lexeme . try) (parseName >>= checkName)
  where
    parseName :: Parser String
    parseName = (:) <$> letterChar <*> many alphaNumChar --проверяет, что сначала буква, потом [a..zA..Z0..9]*
    checkName :: String -> Parser String
    checkName varName = if (varName == "let") || (varName ==  "in") --проверка на то, что слово не зарезервировано
        then fail $ "Name of var should be neither let nor in" 
        else return varName

rword :: String -> Parser ()
rword w = lexeme (string w *> notFollowedBy alphaNumChar) --за словом не следуют другие символы

letParser :: Parser AExpr
letParser = do
    rword "let"
    x <- varNameParserS
    rword "="
    expr <- aExpr
    rword "in"
    sExpr <- aExpr
    return (ALet x expr sExpr)

