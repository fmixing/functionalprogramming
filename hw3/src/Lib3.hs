{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE ExplicitNamespaces         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Lib3
       (
         evaluate
       , Expr (..)
       , Environment (..)
       , ArithmeticError (..)
       , aExpr
       , whileParser
       , StateEnvironment (..)
       , StateError (..)
       , declare
       , update
       , Variable (..)
       , parseProgram
       , evaluateVarExpr'
       , main
       , ParsingError (..)
       , LanguageLexem (..)
       , runProgram
       , LoopError (..)
       ) where

import           System.Environment         (getArgs)
import           Control.Applicative        (liftA2)
import           Control.Monad              (fail)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Reader       (MonadReader, asks, runReader)
import           Control.Monad.State        (MonadIO, MonadState, get,
                                             runStateT, state, modify, evalStateT)
import           Data.Either                (either, isRight)
import           Data.List                  (null)
import qualified Data.Map.Strict            as Map (Map, empty, insert, intersection,
                                                    lookup, fromList)
import           Data.Maybe                 (isJust, isNothing, maybe)
import           Debug.Trace                (trace)

import           Data.Void                  (Void)
import           Text.Megaparsec            (Parsec, between, eof, many, notFollowedBy,
                                             parse, parseTest', try, (<|>))
import           Text.Megaparsec.Char       (alphaNumChar, letterChar, space, string)
import qualified Text.Megaparsec.Char.Lexer as L (lexeme, symbol, decimal)
import           Text.Megaparsec.Expr       (Operator (..), makeExprParser)

----------------First

type VarName = String
type Const = Integer
data Expr = Var VarName | Lit Const | Add Expr Expr |
            Mul Expr Expr | Sub Expr Expr | Div Expr Expr |
            Let VarName Expr Expr | Neg Expr deriving (Show, Eq)

newtype Environment = Environment { dict  :: Map.Map VarName Const } deriving (Show, Eq)

data ParsingError = ArithmError ArithmeticError | VarError StateError | ForError LoopError deriving (Show, Eq)

data ArithmeticError = DivisionByZero | NameNotFound VarName deriving (Show, Eq)

getVarValue :: (MonadReader Environment m) => VarName -> m (Either ParsingError Const)
getVarValue varName = asks ((maybe (Left $ ArithmError $ NameNotFound varName) (Right)) . Map.lookup varName . dict)

evaluateInternal :: (MonadReader Environment m) => Expr -> Expr -> (Const -> Const -> Const) -> m (Either ParsingError Const)
evaluateInternal x y func = asks (\env ->
    let resx = runReader (evaluate x) env
        resy = runReader (evaluate y) env
    in liftA2 func resx resy)

evaluate :: (MonadReader Environment m) => Expr -> m (Either ParsingError Integer)
evaluate (Lit x) = return $ Right x
evaluate (Var name) = getVarValue name
evaluate (Neg x) = evaluateInternal (Lit 0) x (-)
evaluate (Add x y) = evaluateInternal x y (+)
evaluate (Mul x y) = evaluateInternal x y (*)
evaluate (Sub x y) = evaluateInternal x y (-)
evaluate (Div x y) = asks (\env -> let resy = runReader (evaluate y) env in
    resy >>= (\res -> if res == 0 then Left $ ArithmError DivisionByZero else liftA2 div (runReader (evaluate x) env) resy))
evaluate (Let name y z) = asks (\env -> let resy = runReader (evaluate y) env in
    resy >>= (\res -> runReader (evaluate z) (Environment $ Map.insert name res $ dict env)))


----------------Second

-- data ParsecT e s m a
-- e - error, s - stream type, m - underlying monad, a - return type
-- type Parsec e s = ParsecT e s Identity - non-transformer variant of the ParsecT - как Reader от ReaderT
type Parser = Parsec Void String -- e - ошибка - Void, s - stream type - String

whileParser :: Parser Expr
whileParser = between space eof aExpr

aExpr :: Parser Expr
aExpr = makeExprParser aTerm aOperators

aTerm :: Parser Expr
aTerm = parens (aExpr <|> letParser)
    <|> Var <$> varNameParserS
    <|> Lit <$> integer

aOperators :: [[Operator Parser Expr]]
aOperators =
    [[ Prefix  (Neg <$ symbol "-"),
       InfixL  (Mul <$ symbol "*"),
       InfixL  (Div <$ symbol "/")],
     [ InfixL  (Add <$ symbol "+"),
       InfixL  (Sub <$ symbol "-")]]

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
    checkName varName =
        if varName == "let" || varName ==  "in" || varName == "mut" || varName == "for" --проверка на то,
            then fail "Name of var should be neither let nor in, nor mut, nor for"        --что слово не зарезервировано
            else return varName

rword :: String -> Parser ()
rword w = lexeme (string w *> notFollowedBy alphaNumChar) --за словом не следуют другие символы

letParser :: Parser Expr
letParser = do
    rword "let"
    x <- varNameParserS
    _ <- symbol "="
    expr <- aExpr
    rword "in"
    sExpr <- aExpr
    return (Let x expr sExpr)


----------------Third

newtype StateEnvironment = StateEnvironment { stateDict :: Map.Map String Integer } deriving (Show, Eq)

data StateError = NameAlreadyExistsState VarName | NameNotFoundState VarName deriving (Show, Eq)

declare :: (MonadState StateEnvironment m) => String -> Integer -> m (Maybe ParsingError)
declare name value = state (\env -> let mapEnv = stateDict env in
                     if isJust $ Map.lookup name mapEnv
                     then (Just $ VarError $ NameAlreadyExistsState name, env)
                     else (Nothing, StateEnvironment $ Map.insert name value mapEnv))

update :: (MonadState StateEnvironment m) => String -> Integer -> m (Maybe ParsingError)
update name value = state (\env -> let mapEnv = stateDict env in
                     if isNothing $ Map.lookup name mapEnv
                     then (Just $ VarError $ NameNotFoundState name, env)
                     else (Nothing, StateEnvironment $ Map.insert name value mapEnv))

---------------Fourth

data Variable = Declare VarName Expr | Update VarName Expr
              | ConsoleOutput Expr | ConsoleInput VarName deriving (Show, Eq)

data LanguageLexem = VarExpr Variable | For VarName Expr Expr [LanguageLexem] deriving (Show, Eq)

isFor :: LanguageLexem -> Bool
isFor For{} = True
isFor _     = False

isDeclare :: LanguageLexem -> Bool
isDeclare (VarExpr (Declare _ _)) = True
isDeclare _                       = False

isConsole :: LanguageLexem -> Bool
isConsole (VarExpr (ConsoleInput _))  = True
isConsole (VarExpr (ConsoleOutput _)) = True
isConsole _                           = False

isConsoleOutput :: LanguageLexem -> Bool
isConsoleOutput (VarExpr (ConsoleOutput _)) = True
isConsoleOutput _                           = False

getVarName :: LanguageLexem -> VarName
getVarName (VarExpr (Declare varName _))    = varName
getVarName (VarExpr (Update varName _))     = varName
getVarName (VarExpr (ConsoleInput varName)) = varName
getVarName (VarExpr (ConsoleOutput _))      = error "ConsoleOutput expr has no varName"
getVarName (For varName _ _ _)              = varName

getVarExpr :: LanguageLexem -> Expr
getVarExpr (VarExpr (Declare _ varExpr)) = varExpr
getVarExpr (VarExpr (Update _ varExpr)) = varExpr
getVarExpr (VarExpr (ConsoleOutput varExpr)) = varExpr
getVarExpr (VarExpr (ConsoleInput _)) = error "ConsoleInput expr has no varExpr"
getVarExpr For{} = error "For loop has multiple varExpr, please use getVarExprsFor"

parseProgram :: Parser [LanguageLexem]
parseProgram = between space eof parseLanguageLexems

parseLanguageLexems :: Parser [LanguageLexem]
parseLanguageLexems = many (variable <|> forParser)

variable :: Parser LanguageLexem
variable = VarExpr <$> (declarationParser
    <|> updatingParser
    <|> consoleOutputParser
    <|> consoleInputParser)


declarationParser :: Parser Variable
declarationParser = do
    rword "mut"
    x <- varNameParserS
    _ <- symbol "="
    expr <- aExpr
    return (Declare x expr)

updatingParser :: Parser Variable
updatingParser = do
    x <- varNameParserS
    _ <- symbol "="
    expr <- aExpr
    return (Update x expr)

consoleOutputParser :: Parser Variable
consoleOutputParser = do
    _ <- symbol "<"
    expr <- aExpr
    return (ConsoleOutput expr)

consoleInputParser :: Parser Variable
consoleInputParser = do
    _ <- symbol ">"
    name <- varNameParserS
    return (ConsoleInput name)

-- for x = expr to expr {body .... }
forParser :: Parser LanguageLexem
forParser = do
    rword "for"
    name <- varNameParserS
    _ <- symbol "="
    expr1 <- aExpr
    rword "to"
    expr2 <- aExpr
    _ <- symbol "{"
    body <- parseLanguageLexems
    _ <- symbol "}"
    return (For name expr1 expr2 body)

---------------Fifth&Sixth&Seventh

data LoopError = CounterNameAlreadyExists deriving (Show, Eq)

fromRight :: Either a b -> b
fromRight (Right b) = b
fromRight _         = error "I sense something bad"

printConsole :: (MonadState StateEnvironment m, MonadIO m) => LanguageLexem -> m (Maybe ParsingError)
printConsole x = do
    let xExpr = getVarExpr x
    stateEnv <- get
    let xVal = runReader (evaluate xExpr) (Environment $ stateDict stateEnv)
    if isRight xVal 
        then liftIO $ putStrLn $ "< value for expression " ++ show xExpr ++ " is " ++ show xVal
        else return ()
    return $ either Just (const Nothing) xVal

readConsole :: (MonadState StateEnvironment m, MonadIO m) => LanguageLexem -> m (Maybe ParsingError)
readConsole x = do
    let xName = getVarName x
    liftIO $ putStrLn $ "> please print value for variable " ++ getVarName x
    xVal <- liftIO getLine
    stateEnv <- get
    let mapEnv = stateDict stateEnv
    if isNothing $ Map.lookup xName mapEnv
        then declare xName (read xVal)
        else update xName (read xVal)

dealWithConsole :: (MonadState StateEnvironment m, MonadIO m) => LanguageLexem -> m (Maybe ParsingError)
dealWithConsole x = if isConsoleOutput x then printConsole x else readConsole x

evaluateVarExprInternal :: (MonadState StateEnvironment m, MonadIO m) => LanguageLexem -> m (Maybe ParsingError)
evaluateVarExprInternal x = do
    let xExpr = getVarExpr x
    let xName = getVarName x
    stateEnv <- get
    let xVal = runReader (evaluate xExpr) (Environment $ stateDict stateEnv)
    case xVal of
        Left err -> return $ Just err
        Right val -> if isDeclare x then declare xName val else update xName val

runBody :: (MonadState StateEnvironment m, MonadIO m) => VarName -> Integer -> Integer -> [LanguageLexem] -> m (Maybe ParsingError)
runBody counter curCounterVal stopCounterVal body = do
    stateEnv <- get
    let dictProgram = stateDict stateEnv
    if stopCounterVal - curCounterVal > 0 
        then do
            bodyAns <- runStateT (runProgram body) (StateEnvironment $ Map.insert counter curCounterVal $ stateDict stateEnv)
            modify (\_ -> StateEnvironment $ Map.intersection (stateDict $ snd bodyAns) dictProgram)
            case fst bodyAns of
                Just err -> return $ Just err
                Nothing -> runBody counter (curCounterVal + 1) stopCounterVal body
        else return Nothing

sub21 :: (Either ParsingError Integer) -> (Either ParsingError Integer) -> (Either ParsingError Integer)
sub21 x y = do
    xVal <- x
    yVal <- y
    return (yVal - xVal)

runFor :: (MonadState StateEnvironment m, MonadIO m) => LanguageLexem -> m (Maybe ParsingError)
runFor (VarExpr _) = error "runFor function should be used only for For loop"
runFor (For varName aExpr1 aExpr2 body) = do
    stateEnv <- get
    let dictProgram = stateDict stateEnv
    if isJust $ Map.lookup varName dictProgram
        then return $ Just $ ForError CounterNameAlreadyExists
        else do
            let fstVal = runReader (evaluate aExpr1) (Environment dictProgram)
            let sndVal = runReader (evaluate aExpr2) (Environment dictProgram)
            let valExpr = sub21 fstVal sndVal
            case valExpr of
                Left err  -> return $ Just err
                Right _ -> runBody varName (fromRight fstVal) (fromRight sndVal) body


evaluateVarExpr' :: (MonadState StateEnvironment m, MonadIO m) => LanguageLexem -> m (Maybe ParsingError)
evaluateVarExpr' x = if isConsole x then dealWithConsole x else evaluateVarExprInternal x

runProgram :: (MonadState StateEnvironment m, MonadIO m) => [LanguageLexem] -> m (Maybe ParsingError)
runProgram [] = return Nothing
runProgram (x:xs) = do
    ans <- if isFor x
        then runFor x
        else evaluateVarExpr' x
    if null xs || isJust ans
        then return ans
        else runProgram xs

main :: IO ()
main = do
    args <- getArgs
    case args of 
        [file] -> do
            s <- readFile file
            let parsed = parse parseProgram "fact.txt" s
            case parsed of
                Left err -> print err
                Right exprs -> do
                    ansans <- runStateT (runProgram exprs) (StateEnvironment Map.empty)
                    case fst $ ansans of 
                        Just err -> print err 
                        Nothing -> do 
                            putStrLn "Program was succesfully run"
                            print $ snd ansans
        _ -> putStrLn "Wrong args"