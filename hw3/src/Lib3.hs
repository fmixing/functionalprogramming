{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExplicitForAll            #-}
{-# LANGUAGE ExplicitNamespaces        #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
       , StateEnvironment (..)
       , StateError (..)
       , declare
       , update
       , Variable (..)
       , parseVarExpr
       , evaluateVarExpr
       , evaluateVarExpr'
       , main
       , ParsingError (..)
       ) where

import qualified Data.Map.Strict as Map (Map, fromList, insert, lookup, empty)
import Control.Applicative (liftA2)
import Control.Monad.Reader (Reader, asks, runReader, MonadReader)
import Control.Monad.State (State, get, state, runState, liftM, MonadState, MonadIO, StateT, evalState, evalStateT, execStateT)
import Control.Monad (fail)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Maybe (maybe, isJust, isNothing)
import Data.Either (isLeft, either)
import Debug.Trace (trace)
import Data.List (null)
import Control.Monad (void)

import Data.Void (Void)
import Text.Megaparsec (Parsec, notFollowedBy, between, try, many, (<|>), parseTest', parseTest, eof, parse)
import Text.Megaparsec.Char (space, space1, char, string, alphaNumChar, letterChar)
import Text.Megaparsec.Expr (Operator (..), makeExprParser)
import qualified Text.Megaparsec.Char.Lexer as L

----------------First

type VarName = String
type Const = Integer
data Expr = Var VarName | Lit Const | Add Expr Expr | 
            Mul Expr Expr | Sub Expr Expr | Div Expr Expr | 
            Let VarName Expr Expr | Neg Expr deriving (Show, Eq)

data Environment = Environment { dict  :: Map.Map VarName Const }

data ParsingError = ArithmError ArithmeticError | VarError StateError deriving (Show, Eq)

data ArithmeticError = DivisionByZero | NameNotFound deriving (Show, Eq)

getVarValue :: VarName -> Reader Environment (Either ParsingError Const)
getVarValue varName = asks ((maybe (Left $ ArithmError $ NameNotFound) (Right . id)) . Map.lookup varName . dict)

evaluateInternal :: Expr -> Expr -> (Const -> Const -> Const) -> Reader Environment (Either ParsingError Const)
evaluateInternal x y func = asks (\env -> 
    let resx = runReader (evaluate x) env
        resy = runReader (evaluate y) env
    in liftA2 func resx resy)

evaluate :: Expr -> Reader Environment (Either ParsingError Integer)
evaluate (Lit x) = return $ Right x
evaluate (Var name) = getVarValue name
evaluate (Neg x) = evaluateInternal (Lit 0) x (-)
evaluate (Add x y) = evaluateInternal x y (+)
evaluate (Mul x y) = evaluateInternal x y (*)
evaluate (Sub x y) = evaluateInternal x y (-)
evaluate (Div x y) = asks (\env -> let resy = runReader (evaluate y) env in 
    resy >>= (\res -> if res == 0 then Left $ ArithmError $ DivisionByZero else liftA2 (div) (runReader (evaluate x) env) resy))
evaluate (Let name y z) = asks (\env -> let resy = runReader (evaluate y) env in 
    resy >>= (\res -> runReader (evaluate z) (Environment $ Map.insert name res $ dict env)))


----------------Second

convertAExprToExpr :: AExpr -> Expr
convertAExprToExpr (AVar varName) = Var varName
convertAExprToExpr (ALit aConst) = Lit aConst
convertAExprToExpr (ANeg eExpr) = Neg $ convertAExprToExpr eExpr
convertAExprToExpr (ABinary ASub aExpr1 aExpr2) = Sub (convertAExprToExpr aExpr1) (convertAExprToExpr aExpr2)
convertAExprToExpr (ABinary AAdd aExpr1 aExpr2) = Add (convertAExprToExpr aExpr1) (convertAExprToExpr aExpr2)
convertAExprToExpr (ABinary AMul aExpr1 aExpr2) = Mul (convertAExprToExpr aExpr1) (convertAExprToExpr aExpr2)
convertAExprToExpr (ABinary ADiv aExpr1 aExpr2) = Div (convertAExprToExpr aExpr1) (convertAExprToExpr aExpr2)
convertAExprToExpr (ALet varName aExpr1 aExpr2) = Let varName (convertAExprToExpr aExpr1) (convertAExprToExpr aExpr2)


-- x + 3 * (let x = 2 in x / 2)

data AExpr = AVar VarName
           | ALit Const
           | ANeg AExpr
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
    [[ Prefix  (ANeg <$ symbol "-"), 
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
    checkName varName = if (varName == "let") || (varName ==  "in" || varName == "mut") --проверка на то, что слово не зарезервировано
        then fail $ "Name of var should be neither let nor in, nor mut" 
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


----------------Third

data StateEnvironment = StateEnvironment { stateDict :: Map.Map String Integer } deriving (Show)

data StateError = NameAlreadyExistsState | NameNotFoundState deriving (Show, Eq)

declare :: (MonadState StateEnvironment m) => String -> Integer -> m (Maybe ParsingError)
declare name value = state $ (\env -> let mapEnv = stateDict env in 
                     if isJust $ Map.lookup name mapEnv
                     then (Just $ VarError NameAlreadyExistsState, env)
                     else (Nothing, StateEnvironment $ Map.insert name value mapEnv))

update :: (MonadState StateEnvironment m) => String -> Integer -> m (Maybe ParsingError)
update name value = state $ (\env -> let mapEnv = stateDict env in 
                     if isNothing $ Map.lookup name mapEnv
                     then (Just $ VarError NameNotFoundState, env)
                     else (Nothing, StateEnvironment $ Map.insert name value mapEnv))

---------------Fourth

data Variable = Declare VarName AExpr | Update VarName AExpr | Console AExpr deriving (Show, Eq)

isDeclare :: Variable -> Bool
isDeclare (Declare _ _) = True
isDeclare _ = False

isConsole :: Variable -> Bool
isConsole (Console _) = True
isConsole _ = False

getVarName :: Variable -> VarName
getVarName (Declare varName _) = varName
getVarName (Update varName _) = varName
getVarName (Console _) = error "Console expr has no varName"

getVarExpr :: Variable -> AExpr
getVarExpr (Declare _ varExpr) = varExpr
getVarExpr (Update _ varExpr) = varExpr
getVarExpr (Console varExpr) = varExpr
 
parseVarExpr :: Parser Variable
parseVarExpr = between space eof variable

variable :: Parser Variable
variable = declarationParser
    <|> updatingParser
    <|> consoleParser

declarationParser :: Parser Variable
declarationParser = do
    rword "mut"
    x <- varNameParserS
    rword "="
    expr <- aExpr
    return (Declare x expr)

updatingParser :: Parser Variable
updatingParser = do
    x <- varNameParserS
    rword "="
    expr <- aExpr
    return (Update x expr)

consoleParser :: Parser Variable
consoleParser = do
    rword "<"
    expr <- aExpr
    return (Console expr)

---------------Fifth&Sixth

fromLeft :: Either a b -> a
fromLeft (Left a) = a
fromLeft _ = error "I sense something bad"

fromRight :: Either a b -> b
fromRight (Right b) = b
fromRight _ = error "I sense something bad"

printConsole :: (MonadState StateEnvironment m, MonadIO m) => Variable -> m (Maybe ParsingError)
printConsole x = do 
    let xExpr = getVarExpr x
    stateEnv <- get
    let xVal = runReader (evaluate $ convertAExprToExpr xExpr) (Environment $ stateDict stateEnv)
    liftIO $ putStrLn $ "< value for expression " ++ show xExpr ++ " is " ++ show xVal
    return $ either (\err -> Just err) (\_ -> Nothing) xVal

evaluateVarExprInternal :: (MonadState StateEnvironment m, MonadIO m) => Variable -> m (Maybe ParsingError)
evaluateVarExprInternal x = do
    let xExpr = getVarExpr x
    let xName = getVarName x
    stateEnv <- get
    let xVal = runReader (evaluate $ convertAExprToExpr xExpr) (Environment $ stateDict stateEnv)
    ans <- case isLeft xVal of
        True -> return $ Just $ fromLeft xVal
        False -> case isDeclare x of 
            True -> declare xName $ fromRight xVal
            False -> update xName $ fromRight xVal
    return ans

---ONLY FOR TESTS
evaluateVarExpr :: [Variable] -> State StateEnvironment (Maybe ParsingError)
evaluateVarExpr [] = return Nothing
evaluateVarExpr (x:xs) = do
    let xName = getVarName x
    let xExpr = getVarExpr x
    stateEnv <- get
    let xVal = runReader (evaluate $ convertAExprToExpr xExpr) (Environment $ stateDict stateEnv)
    ans <- case isLeft xVal of
        True -> return $ Just $ fromLeft xVal
        False -> case isDeclare x of 
            True -> declare xName $ fromRight xVal
            False -> update xName $ fromRight xVal
    if (not $ null xs) && (isNothing ans) then evaluateVarExpr xs else return ans

evaluateVarExpr' :: (MonadState StateEnvironment m, MonadIO m) => [Variable] -> m ()
evaluateVarExpr' [] = return ()
evaluateVarExpr' (x:xs) = do
    ans <- if isConsole x 
        then printConsole x
        else evaluateVarExprInternal x
    if (not $ null xs) then evaluateVarExpr' xs else liftIO $ print ans 

main :: IO ()
main = do
    let upd = Update "x" (ALet "x" (ALit 2) (AVar "x")) 
    let dec = Declare "x" (ALit 1)
    let bad = Declare "x" (ABinary AAdd (AVar "x") (ABinary ADiv (ABinary AMul (ALit 3) (ALit 2)) (ALit 0)))
    let console = Console (ABinary AAdd (AVar "y") (ALit 1))
    -- putStrLn $ show (runState (evaluateVarExpr [dec, dec]) (StateEnvironment $ Map.empty))
    ans <- execStateT (evaluateVarExpr' [dec, console]) (StateEnvironment $ Map.empty)
    print ans