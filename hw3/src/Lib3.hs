{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExplicitForAll            #-}
{-# LANGUAGE ExplicitNamespaces        #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}

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

import           Control.Monad              (fail, when)
import           Control.Monad.Except       (MonadError, runExceptT, throwError)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Reader       (MonadReader, ask, runReaderT)
import           Control.Monad.State        (MonadIO, MonadState, evalStateT, execStateT,
                                             get, modify, runStateT)
import           Data.List                  (null)
import qualified Data.Map.Strict            as Map (Map, empty, fromList, insert,
                                                    intersection, lookup)
import           Data.Maybe                 (isJust, isNothing)
import           Debug.Trace                (trace)
import           System.Environment         (getArgs)

import           Data.Text                  (Text, append, pack, unpack)
import           Data.Void                  (Void)
import           Text.Megaparsec            (Parsec, between, eof, many, notFollowedBy,
                                             parse, parseTest', try, (<|>))
import           Text.Megaparsec.Char       (alphaNumChar, letterChar, space, string)
import qualified Text.Megaparsec.Char.Lexer as L (decimal, lexeme, symbol)
import           Text.Megaparsec.Expr       (Operator (..), makeExprParser)

----------------First

type VarName = Text
type Const = Integer
data Expr = Var VarName | Lit Const | Add Expr Expr |
            Mul Expr Expr | Sub Expr Expr | Div Expr Expr |
            Let VarName Expr Expr | Neg Expr deriving (Show, Eq)

newtype Environment = Environment { dict  :: Map.Map VarName Const } deriving (Show, Eq)

data ParsingError = ArithmError ArithmeticError | VarError StateError | ForError LoopError deriving (Show, Eq)

data ArithmeticError = DivisionByZero | NameNotFound VarName deriving (Show, Eq)

getVarValue :: (MonadReader Environment m, MonadError ParsingError m) => VarName -> m Const
getVarValue varName = do
    env <- ask
    let dictReader = dict env
    let val = Map.lookup varName dictReader
    -- val <- asks (Map.lookup varName . dict)
    -- asks (Map.lookup varName . dict) >>= \case
    case val of
        Nothing -> throwError $ ArithmError $ NameNotFound varName
        Just x  -> return x

evaluateInternal :: (MonadReader Environment m, MonadError ParsingError m)
                 => Expr -> Expr -> (Const -> Const -> Const) -> m Const
evaluateInternal x y func = do -- liftM2 func (evaluate x) (evaluate y)
    resx <- evaluate x
    resy <- evaluate y
    return (func resx resy)

evaluate :: (MonadReader Environment m, MonadError ParsingError m) => Expr -> m Const
evaluate (Lit x) = return x
evaluate (Var name) = getVarValue name
evaluate (Neg x) = evaluateInternal (Lit 0) x (-) -- fmap negate (evaluate x)
evaluate (Add x y) = evaluateInternal x y (+)
evaluate (Mul x y) = evaluateInternal x y (*)
evaluate (Sub x y) = evaluateInternal x y (-)
evaluate (Div x y) = do
    resy <- evaluate y
    if resy == 0
        then throwError $ ArithmError DivisionByZero
        else do
            resx <- evaluate x
            return $ resx `div` resy
evaluate (Let name y z) = do
    env <- ask
    resy <- evaluate y
    runReaderT (evaluate z) (Environment $ Map.insert name resy $ dict env)


----------------Second

-- data ParsecT e s m a
-- e - error, s - stream type, m - underlying monad, a - return type
-- type Parsec e s = ParsecT e s Identity - non-transformer variant of the ParsecT - как Reader от ReaderT
type Parser = Parsec Void Text -- e - ошибка - Void, s - stream type - Text

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

symbol :: Text -> Parser Text
symbol = L.symbol space

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

integer :: Parser Integer
integer = lexeme L.decimal

varNameParserS :: Parser Text
varNameParserS = (lexeme . try) (parseName >>= checkName)
  where
    parseName :: Parser Text
    parseName = pack <$> parseName'
    parseName' = (:) <$> letterChar <*> many alphaNumChar --проверяет, что сначала буква, потом [a..zA..Z0..9]*
    checkName :: Text -> Parser Text
    checkName varName =
        if varName `elem` ["let", "in", "mut", "for"] --проверка на то, что слово не зарезервировано
            then fail "Name of var should be neither let nor in, nor mut, nor for"
            else return varName

rword :: Text -> Parser ()
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

newtype StateEnvironment = StateEnvironment { stateDict :: Map.Map Text Integer } deriving (Show, Eq)

data StateError = NameAlreadyExistsState VarName | NameNotFoundState VarName deriving (Show, Eq)

declare :: (MonadState StateEnvironment m, MonadError ParsingError m) => Text -> Integer -> m ()
declare name value = do
    env <- get
    let mapEnv = stateDict env
    case Map.lookup name mapEnv of
        Just _  -> throwError $ VarError $ NameAlreadyExistsState name
        Nothing -> modify (\_ -> StateEnvironment $ Map.insert name value mapEnv)

update :: (MonadState StateEnvironment m, MonadError ParsingError m) => Text -> Integer -> m ()
update name value = do
    env <- get
    let mapEnv = stateDict env
    case Map.lookup name mapEnv of
        Nothing -> throwError $ VarError $ NameNotFoundState name
        Just _  -> modify (\_ -> StateEnvironment $ Map.insert name value mapEnv)
-- fix дублирование

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

printConsole :: (MonadState StateEnvironment m, MonadIO m, MonadError ParsingError m) => LanguageLexem -> m ()
printConsole x = do
    let xExpr = getVarExpr x
    stateEnv <- get
    xVal <- runReaderT (evaluate xExpr) (Environment $ stateDict stateEnv)
    liftIO $ putStrLn $ "< value for expression " ++ show xExpr ++ " is " ++ show xVal

readConsole :: (MonadState StateEnvironment m, MonadIO m, MonadError ParsingError m) => LanguageLexem -> m ()
readConsole x = do
    let xName = getVarName x
    liftIO $ putStrLn $ unpack $ "> please print value for variable " `append` getVarName x -- <>, Data.Text.IO 
    xVal <- liftIO getLine
    stateEnv <- get
    let mapEnv = stateDict stateEnv
    if isNothing $ Map.lookup xName mapEnv
        then declare xName (read xVal)
        else update xName (read xVal)

dealWithConsole :: (MonadState StateEnvironment m, MonadIO m, MonadError ParsingError m) => LanguageLexem -> m ()
dealWithConsole x = if isConsoleOutput x then printConsole x else readConsole x

evaluateVarExprInternal :: (MonadState StateEnvironment m, MonadIO m, MonadError ParsingError m) => LanguageLexem -> m ()
evaluateVarExprInternal x = do
    let xExpr = getVarExpr x
    let xName = getVarName x
    stateEnv <- get
    xVal <- runReaderT (evaluate xExpr) (Environment $ stateDict stateEnv)
    if isDeclare x then declare xName xVal else update xName xVal

runBody :: (MonadState StateEnvironment m, MonadIO m, MonadError ParsingError m)
        => VarName -> Integer -> Integer -> [LanguageLexem] -> m ()
runBody counter curCounterVal stopCounterVal body = do
    stateEnv <- get
    let dictProgram = stateDict stateEnv
    when (stopCounterVal - curCounterVal > 0) $ do
        bodyAns <- runStateT (runProgram body) (StateEnvironment $ Map.insert counter curCounterVal $ stateDict stateEnv)
        modify (\_ -> StateEnvironment $ Map.intersection (stateDict $ snd bodyAns) dictProgram)
        runBody counter (curCounterVal + 1) stopCounterVal body


runFor :: (MonadState StateEnvironment m, MonadIO m, MonadError ParsingError m) => LanguageLexem -> m ()
runFor (VarExpr _) = error "runFor function should be used only for For loop"
runFor (For varName aExpr1 aExpr2 body) = do
    stateEnv <- get
    let dictProgram = stateDict stateEnv
    if isJust $ Map.lookup varName dictProgram
        then throwError $ ForError CounterNameAlreadyExists
        else do
            fstVal <- runReaderT (evaluate aExpr1) (Environment dictProgram)
            sndVal <- runReaderT (evaluate aExpr2) (Environment dictProgram)
            runBody varName fstVal sndVal body


evaluateVarExpr' :: (MonadState StateEnvironment m, MonadIO m, MonadError ParsingError m) => LanguageLexem -> m ()
evaluateVarExpr' x = if isConsole x then dealWithConsole x else evaluateVarExprInternal x

runProgram :: (MonadState StateEnvironment m, MonadIO m, MonadError ParsingError m) => [LanguageLexem] -> m ()
runProgram [] = return ()
runProgram (x:xs) = do
    if isFor x
        then runFor x
        else evaluateVarExpr' x
    if null xs
        then return ()
        else runProgram xs


main :: IO ()
main = do
    args <- getArgs
    case args of
        [file] -> do
            s <- readFile file
            let parsed = parse parseProgram file $ pack s
            case parsed of
                Left err -> print err
                Right exprs -> do
                    ansans <- runExceptT (runStateT (runProgram exprs) (StateEnvironment Map.empty))
                    case ansans of
                        Left err -> print err
                        Right ans -> do
                            putStrLn "Program was succesfully run"
                            print ans
        _ -> putStrLn "Wrong args"