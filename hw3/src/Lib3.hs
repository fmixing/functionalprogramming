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
    --    , AExpr (..)
    --    , ABinOp (..)
       , StateEnvironment (..)
       , StateError (..)
       , declare
       , update
       , Variable (..)
       , parseProgram
    --    , evaluateVarExpr
       , evaluateVarExpr'
       , main
       , ParsingError (..)
       ) where

import qualified Data.Map.Strict as Map (Map, insert, lookup, empty, intersection)
import Control.Applicative (liftA2)
import Control.Monad.Reader (Reader, asks, runReader, MonadReader)
import Control.Monad.State (State, get, state, MonadState, MonadIO, execStateT, runStateT)
import Control.Monad (fail)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Maybe (maybe, isJust, isNothing)
import Data.Either (either)
import Debug.Trace (trace)
import Data.List (null)

import Data.Void (Void)
import Text.Megaparsec (Parsec, notFollowedBy, between, try, many, (<|>), parseTest', parseTest, eof, parse)
import Text.Megaparsec.Char (space, space1, char, string, alphaNumChar, letterChar, newline)
import Text.Megaparsec.Expr (Operator (..), makeExprParser)
import qualified Text.Megaparsec.Char.Lexer as L

----------------First

type VarName = String
type Const = Integer
data Expr = Var VarName | Lit Const | Add Expr Expr | 
            Mul Expr Expr | Sub Expr Expr | Div Expr Expr | 
            Let VarName Expr Expr | Neg Expr deriving (Show, Eq)

data Environment = Environment { dict  :: Map.Map VarName Const }

data ParsingError = ArithmError ArithmeticError | VarError StateError | ForError CycleError deriving (Show, Eq)

data ArithmeticError = DivisionByZero | NameNotFound deriving (Show, Eq)

getVarValue :: (MonadReader Environment m) => VarName -> m (Either ParsingError Const)
getVarValue varName = asks ((maybe (Left $ ArithmError $ NameNotFound) (Right . id)) . Map.lookup varName . dict)

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
    resy >>= (\res -> if res == 0 then Left $ ArithmError $ DivisionByZero else liftA2 (div) (runReader (evaluate x) env) resy))
evaluate (Let name y z) = asks (\env -> let resy = runReader (evaluate y) env in 
    resy >>= (\res -> runReader (evaluate z) (Environment $ Map.insert name res $ dict env)))


----------------Second

-- convertAExprToExpr :: AExpr -> Expr
-- convertAExprToExpr (AVar varName) = Var varName
-- convertAExprToExpr (ALit aConst) = Lit aConst
-- convertAExprToExpr (ANeg eExpr) = Neg $ convertAExprToExpr eExpr
-- convertAExprToExpr (ABinary ASub aExpr1 aExpr2) = Sub (convertAExprToExpr aExpr1) (convertAExprToExpr aExpr2)
-- convertAExprToExpr (ABinary AAdd aExpr1 aExpr2) = Add (convertAExprToExpr aExpr1) (convertAExprToExpr aExpr2)
-- convertAExprToExpr (ABinary AMul aExpr1 aExpr2) = Mul (convertAExprToExpr aExpr1) (convertAExprToExpr aExpr2)
-- convertAExprToExpr (ABinary ADiv aExpr1 aExpr2) = Div (convertAExprToExpr aExpr1) (convertAExprToExpr aExpr2)
-- convertAExprToExpr (ALet varName aExpr1 aExpr2) = Let varName (convertAExprToExpr aExpr1) (convertAExprToExpr aExpr2)

-- convertAExprToExpr :: AExpr -> Expr
-- convertAExprToExpr k = k
-- convertAExprToExpr (AVar varName) = Var varName
-- convertAExprToExpr (ALit aConst) = Lit aConst
-- convertAExprToExpr (ANeg eExpr) = Neg $ convertAExprToExpr eExpr
-- convertAExprToExpr (ASub aExpr1 aExpr2) = Sub (convertAExprToExpr aExpr1) (convertAExprToExpr aExpr2)
-- convertAExprToExpr (AAdd aExpr1 aExpr2) = Add (convertAExprToExpr aExpr1) (convertAExprToExpr aExpr2)
-- convertAExprToExpr (AMul aExpr1 aExpr2) = Mul (convertAExprToExpr aExpr1) (convertAExprToExpr aExpr2)
-- convertAExprToExpr (ADiv aExpr1 aExpr2) = Div (convertAExprToExpr aExpr1) (convertAExprToExpr aExpr2)
-- convertAExprToExpr (ALet varName aExpr1 aExpr2) = Let varName (convertAExprToExpr aExpr1) (convertAExprToExpr aExpr2)


-- x + 3 * (let x = 2 in x / 2)

-- data AExpr = AVar VarName
--            | ALit Const
--            | ANeg AExpr
--            | ABinary ABinOp AExpr AExpr
--            | ALet VarName AExpr AExpr
--            deriving (Show, Eq)

-- data AExpr = AVar VarName
--            | ALit Const
--            | ANeg AExpr
--            | AAdd AExpr AExpr
--            | ASub AExpr AExpr
--            | AMul AExpr AExpr
--            | ADiv AExpr AExpr
--            | ALet VarName AExpr AExpr
--            deriving (Show, Eq)

-- data ABinOp = AAdd
--             | ASub
--             | AMul
--             | ADiv
--             deriving (Show, Eq)

-- data ParsecT e s m a
-- e - error, s - stream type, m - underlying monad, a - return type
-- type Parsec e s = ParsecT e s Identity - non-transformer variant of the ParsecT - как Reader от ReaderT
type Parser = Parsec Void String -- e - ошибка - Void, s - stream type - String

whileParser :: Parser Expr
whileParser = between space eof aExpr

aExpr :: Parser Expr
aExpr = makeExprParser aTerm aOperators

aTerm :: Parser Expr
aTerm = parens letParser
    <|> Var <$> varNameParserS
    <|> Lit <$> integer
    <|> parens aExpr

aOperators :: [[Operator Parser Expr]]
aOperators = 
    [[ Prefix  (Neg <$ symbol "-"), 
       InfixL  ((Mul) <$ symbol "*"),
       InfixL  ((Div) <$ symbol "/")],
     [ InfixL  ((Add) <$ symbol "+"),
       InfixL  ((Sub) <$ symbol "-")]]

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
        if (varName == "let" || varName ==  "in" || varName == "mut" || varName == "for") --проверка на то, 
            then fail $ "Name of var should be neither let nor in, nor mut, nor for"      --что слово не зарезервировано
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

data Variable = Declare VarName Expr | Update VarName Expr
              | ConsoleOutput Expr | ConsoleInput VarName deriving (Show, Eq)

data LanguageLexem = VarExpr Variable | For VarName Expr Expr [LanguageLexem] deriving (Show, Eq)

isFor :: LanguageLexem -> Bool
isFor (For _ _ _ _) = True
isFor _ = False

isDeclare :: LanguageLexem -> Bool
isDeclare (VarExpr (Declare _ _)) = True
isDeclare _ = False

isConsole :: LanguageLexem -> Bool
isConsole (VarExpr (ConsoleInput _)) = True
isConsole (VarExpr (ConsoleOutput _)) = True
isConsole _ = False

isConsoleOutput :: LanguageLexem -> Bool
isConsoleOutput (VarExpr (ConsoleOutput _)) = True
isConsoleOutput _ = False

getVarName :: LanguageLexem -> VarName
getVarName (VarExpr (Declare varName _)) = varName
getVarName (VarExpr (Update varName _)) = varName
getVarName (VarExpr (ConsoleInput varName)) = varName
getVarName (VarExpr (ConsoleOutput _)) = error "ConsoleOutput expr has no varName"
getVarName (For varName _ _ _) = varName

getVarExpr :: LanguageLexem -> Expr
getVarExpr (VarExpr (Declare _ varExpr)) = varExpr
getVarExpr (VarExpr (Update _ varExpr)) = varExpr
getVarExpr (VarExpr (ConsoleOutput varExpr)) = varExpr
getVarExpr (VarExpr (ConsoleInput _)) = error "ConsoleInput expr has no varExpr"
getVarExpr (For _ _ _ _) = error "For cycle has multiple varExpr, please use getVarExprsFor"

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

data CycleError = CounterNameAlreadyExists | NegativeCycleCount deriving (Show, Eq)

fromRight :: Either a b -> b
fromRight (Right b) = b
fromRight _ = error "I sense something bad"

printConsole :: (MonadState StateEnvironment m, MonadIO m) => LanguageLexem -> m (Maybe ParsingError)
printConsole x = do
    let xExpr = getVarExpr x
    stateEnv <- get
    -- let xVal = runReader (evaluate $ convertAExprToExpr xExpr) (Environment $ stateDict stateEnv)
    let xVal = runReader (evaluate xExpr) (Environment $ stateDict stateEnv)    
    liftIO $ putStrLn $ "< value for expression " ++ show xExpr ++ " is " ++ show xVal
    return $ either (\err -> Just err) (\_ -> Nothing) xVal

readConsole :: (MonadState StateEnvironment m, MonadIO m) => LanguageLexem -> m (Maybe ParsingError)
readConsole x = do
    let xName = getVarName x
    liftIO $ putStrLn $ "> please print value for variable " ++ getVarName x
    xVal <- liftIO $ getLine
    stateEnv <- get
    let mapEnv = stateDict stateEnv 
    if isNothing $ Map.lookup xName mapEnv
        then declare xName (read xVal)
        else update xName (read xVal)

dealWithConsole :: (MonadState StateEnvironment m, MonadIO m) => LanguageLexem -> m (Maybe ParsingError)
dealWithConsole x = do 
    if isConsoleOutput x then printConsole x else readConsole x

evaluateVarExprInternal :: (MonadState StateEnvironment m, MonadIO m) => LanguageLexem -> m (Maybe ParsingError)
evaluateVarExprInternal x = do
    let xExpr = getVarExpr x
    let xName = getVarName x
    stateEnv <- get
    let xVal = runReader (evaluate xExpr) (Environment $ stateDict stateEnv)
    case xVal of
        Left err -> return $ Just err
        Right val -> case isDeclare x of 
            True -> declare xName val
            False -> update xName val

runBody :: (MonadState StateEnvironment m, MonadIO m) => VarName -> Integer -> Integer -> [LanguageLexem] -> m (Maybe ParsingError)
runBody counter counterVal n body = do
    stateEnv <- get
    let dictProgram = stateDict stateEnv
    case n >= 0 of
        True -> do
            let count = fromIntegral n
            let lexems = concat $ replicate count (body ++ [VarExpr (Update counter (Add (Var counter) (Lit 1)))])
            bodyAns <- runStateT (runProgram lexems) (StateEnvironment $ Map.insert counter counterVal $ stateDict stateEnv)
            state $ (\_ -> (fst bodyAns, StateEnvironment $ Map.intersection (stateDict $ snd bodyAns) dictProgram))
        False -> return $ Just $ ForError NegativeCycleCount

sub21 :: (Either ParsingError Integer) -> (Either ParsingError Integer) -> (Either ParsingError Integer)
sub21 x y = do
    xVal <- x
    yVal <- y
    return (yVal - xVal)

runFor :: (MonadState StateEnvironment m, MonadIO m) => LanguageLexem -> m (Maybe ParsingError)
runFor (For varName aExpr1 aExpr2 body) = do 
    stateEnv <- get
    let dictProgram = stateDict stateEnv
    if isJust $ Map.lookup varName dictProgram 
        then return $ Just $ ForError $ CounterNameAlreadyExists
        else do
            let fstVal = runReader (evaluate aExpr1) (Environment dictProgram)
            let sndVal = runReader (evaluate aExpr2) (Environment dictProgram)
            let valExpr = sub21 fstVal sndVal
            case valExpr of
                Left err -> return $ Just err
                Right val -> runBody varName (fromRight fstVal) val body

    
evaluateVarExpr' :: (MonadState StateEnvironment m, MonadIO m) => LanguageLexem -> m (Maybe ParsingError)
evaluateVarExpr' x = do
    if isConsole x then dealWithConsole x else evaluateVarExprInternal x

runProgram :: (MonadState StateEnvironment m, MonadIO m) => [LanguageLexem] -> m (Maybe ParsingError)
runProgram [] = return Nothing
runProgram (x:xs) = do 
    ans <- if isFor x 
        then runFor x 
        else evaluateVarExpr' x
    if not $ null xs && isNothing ans
        then runProgram xs 
        else do
            liftIO $ print ans 
            return ans

main :: IO ()
main = do
    s <- readFile "fact.txt"
    let parsed = parse parseProgram "fact.txt" s
    case parsed of
        Left err -> print err
        Right exprs -> do
            ans <- execStateT (runProgram exprs) (StateEnvironment $ Map.empty)
            print ans

-- main :: IO ()
-- main = do
--     -- let upd = VarExpr (Update "x" (ALet "x" (ALit 2) (AVar "x")))
--     -- let dec = VarExpr (Declare "x" (ALit 1))
--     -- let bad = VarExpr (Declare "x" (ABinary AAdd (AVar "x") (ABinary ADiv (ABinary AMul (ALit 3) (ALit 2)) (ALit 0))))
--     -- let consoleOut = VarExpr (ConsoleOutput (ABinary AAdd (AVar "x") (ALit 1)))
--     -- let cI = VarExpr (ConsoleInput "x")
--     -- let forBad = [For "x" (ALit 1) (ALit 2) [VarExpr (ConsoleInput "x")], VarExpr (ConsoleOutput (AVar "x"))]
--     let for = [VarExpr (Declare "x" (ALit 5)),For "i" (ALit 1) (ALit 4) [VarExpr (ConsoleOutput (ABinary AAdd (AVar "x") (AVar "i")))],VarExpr (ConsoleOutput (AVar "x"))]
--     let fact = [VarExpr (Declare "x" (ALit 1)),For "i" (ALit 1) (ALit 5) [VarExpr (ConsoleOutput (ABinary AMul (AVar "x") (AVar "i"))),VarExpr (Update "x" (ABinary AMul (AVar "x") (AVar "i")))],VarExpr (ConsoleOutput (AVar "x")),VarExpr (ConsoleInput "x")]
--     -- putStrLn $ show (runState (evaluateVarExpr [dec, dec]) (StateEnvironment $ Map.empty))
--     ans <- execStateT (runProgram fact) (StateEnvironment $ Map.empty)
--     -- ans <- execStateT (evaluateVarExpr' [cI]) (StateEnvironment $ Map.empty) 
--     print ans


---ONLY FOR TESTS
-- evaluateVarExpr :: [Variable] -> State StateEnvironment (Maybe ParsingError)
-- evaluateVarExpr [] = return Nothing
-- evaluateVarExpr (x:xs) = do
--     let xName = getVarName x
--     let xExpr = getVarExpr x
--     stateEnv <- get
--     let xVal = runReader (evaluate $ convertAExprToExpr xExpr) (Environment $ stateDict stateEnv)
--     ans <- case isLeft xVal of
--         True -> return $ Just $ fromLeft xVal
--         False -> case isDeclare x of 
--             True -> declare xName $ fromRight xVal
--             False -> update xName $ fromRight xVal
--     if (not $ null xs) && (isNothing ans) then evaluateVarExpr xs else return ans


-- evaluateVarExpr' :: (MonadState StateEnvironment m, MonadIO m) => [LanguageLexem] -> m ()
-- evaluateVarExpr' [] = return ()
-- evaluateVarExpr' (x:xs) = do
--     ans <- if isConsole x 
--         then dealWithConsole x
--         else evaluateVarExprInternal x
--     if (not $ null xs) && (isNothing ans) then evaluateVarExpr' xs else liftIO $ print ans 