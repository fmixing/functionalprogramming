{-# LANGUAGE OverloadedStrings          #-}

module TestSpec (spec) where

import           Test.Hspec
import qualified Data.List (sortBy, permutations)
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Data.Char
import           Data.Text                  (Text, append, pack, unpack)

import qualified Data.Map.Strict as Map (Map, fromList, empty)
import           Lib3
import Control.Monad.Reader (runReader, runReaderT)
import Text.Megaparsec (parse)
import Text.Megaparsec.Error (ParseError (..))
import Data.Either (isLeft, isRight)
import Data.Maybe (isNothing, isJust, fromJust)
import Control.Monad.State (State, get, modify, state, runState, evalStateT, execStateT, runStateT)
import Control.Monad.Except (MonadError, catchError, throwError, ExceptT, runExceptT)


evaluateRight = Var "x" `Add` (Lit 3 `Mul` ("x" `Let` (Div (Lit 5) (Lit 2)) $ Var "x"))
evaluateWrong = Var "x" `Add` (Lit 3 `Mul` ("x" `Let` (Div (Lit 5) (Lit 0)) $ Var "x"))

parseRight = Add (Var "x") (Mul (Lit 3) (Let "x" (Mul (Var "y") (Lit 3)) (Div (Var "x") (Lit 2))))

fact = pack "mut x = 1\nfor i = 1 to 5 {\n    x = x * i\n}"
counterNameTest = pack "mut x = 1\nfor x = 1 to 5 {\n    x = x * x\n}"
    
spec :: Spec
spec = do
    describe "Evaluate ArithmExpr" $ do
        it "x + 3 * (let x = 5 / 2 in x) with (Env x = 1) -> 7" $ do
            ans <- runExceptT (runReaderT (evaluate evaluateRight) (Environment $ Map.fromList [("x", 1)]))
            case ans of 
                Right ansans -> ansans `shouldBe` 7  
        it "x + 3 * (let x = 5 / 0 in x) with (Env x = 1) -> DivisionByZero" $ do
            ans <- runExceptT (runReaderT (evaluate evaluateWrong) (Environment $ Map.fromList [("x", 1)])) 
            ans `shouldBe` Left (ArithmError DivisionByZero)
        it "x + 3 * (let x = 5 / 0 in x) with (Env y = 1) -> NameNotFound" $ do
            ans <- runExceptT (runReaderT (evaluate evaluateRight) (Environment $ Map.fromList [("y", 1)])) 
            ans `shouldBe` Left (ArithmError (NameNotFound "x"))
        it "x + 3 * (let x = 5 / 0 in x) with (Env is empty) -> NameNotFound" $ do
            ans <- runExceptT (runReaderT (evaluate evaluateRight) (Environment $ Map.fromList [])) 
            ans `shouldBe` Left (ArithmError (NameNotFound "x"))
        it "2 / x with (Env x = 0) -> DivisionByZero" $ do
            ans <- runExceptT (runReaderT (evaluate (Div (Lit 2) (Var "x"))) (Environment $ Map.fromList [("x", 0)])) 
            ans `shouldBe` Left (ArithmError DivisionByZero)
        it "2 / x with (Env x = 1) -> 2" $ do
            ans <- runExceptT (runReaderT (evaluate (Div (Lit 2) (Var "x"))) (Environment $ Map.fromList [("x", 1)])) 
            case ans of 
                Right ansans -> ansans `shouldBe` 2
    describe "Parse ArithmExpr" $ do
        it "x + 3 * (let x = y * 3 in x / 2)" $ do
            parse whileParser "fileName" "x + 3 * (let x = y * 3 in x / 2)" `shouldBe` Right parseRight
        it "let x = 2 in x" $ do
            parse whileParser "fileName" "let x = 2 in x" `shouldSatisfy` isLeft
        it "(let x = 2 in x" $ do
            parse whileParser "fileName" "(let x = 2 in x" `shouldSatisfy` isLeft
    describe "Parse then evaluate ArithmExpr" $ do
        it "10" $
            case parse whileParser "fileName" "10" of
                (Right r) -> do
                    ans <- runExceptT (runReaderT (evaluate r) (Environment $ Map.fromList [])) 
                    case ans of 
                        Right ansans -> ansans `shouldBe` 10
        it "((((x + 10))) * (((x - 10)))) + ((let x = 0 in x + x))" $
            case parse whileParser "fileName" "((((x + 10))) * (((x - 10)))) + ((let x = 0 in x + x))" of
                (Right r) -> do
                    ans <- runExceptT (runReaderT (evaluate r) (Environment $ Map.fromList [("x", 11)])) 
                    case ans of 
                        Right ansans -> ansans `shouldBe` 21
        it "(2 * 10) + (let x = 10 in (x * 4)) / 0" $
            case parse whileParser "fileName" "(((2 * 10))) + (let x = 10 in (x * 4)) / 0" of
                (Right r) -> do
                    ans <- runExceptT (runReaderT (evaluate r) (Environment $ Map.fromList [("x", 11)]))
                    ans `shouldBe` Left (ArithmError DivisionByZero)
    describe "Declare and update" $ do
        it "declare y = 1 with (Env x = 1) -> Nothing" $ do
            ans <- runExceptT (evalStateT (declare "y" 1) (StateEnvironment $ Map.fromList [("x", 1)])) 
            ans `shouldSatisfy` isRight
        it "declare y = 1 with (Env y = 1) -> NameAlreadyExistsState" $ do
            ans <- runExceptT (evalStateT (declare "y" 1) (StateEnvironment $ Map.fromList [("y", 1)])) 
            ans`shouldBe` Left (VarError (NameAlreadyExistsState "y"))
        it "update y = 2 with (Env y = 1) -> Nothing" $ do
            ans <- runExceptT (evalStateT (update "y" 2) (StateEnvironment $ Map.fromList [("y", 1)])) 
            ans `shouldSatisfy` isRight
        it "update y = 2 with (Env x = 1) -> NameNotFoundState" $ do
            ans <- runExceptT (evalStateT (update "y" 2) (StateEnvironment $ Map.fromList [("x", 1)])) 
            ans`shouldBe` Left (VarError (NameNotFoundState "y"))
    describe "Parse LanguageLexem" $ do
        it "mut x = 1" $
            parse parseProgram "fileName" "mut x = 1" `shouldBe` Right ([VarExpr (Declare "x" (Lit 1))])   
        it "x = 1" $
            parse parseProgram "fileName" "x = 1" `shouldBe` Right ([VarExpr (Update "x" (Lit 1))])    
        it "mut x = 5\\n x = 10\\n" $
            parse parseProgram "fileName" "mut x = 5\n x = 10\n" `shouldBe` Right ([VarExpr (Declare "x" (Lit 5)),VarExpr (Update "x" (Lit 10))])
        it "> x" $
            parse parseProgram "fileName" "> x" `shouldBe` Right ([VarExpr (ConsoleInput "x")])
        it "< x + 3" $
            parse parseProgram "fileName" "< x + 3" `shouldBe` Right ([VarExpr (ConsoleOutput (Add (Var "x") (Lit 3)))])
        it "mutx x=10" $
            parse parseProgram "fileName" "mutx x=10" `shouldSatisfy` isLeft
        it "for x=1 to 2{>x}" $
            parse parseProgram "fileName" "for x=1 to 2{>x}" `shouldBe` Right ([For "x" (Lit 1) (Lit 2) [VarExpr (ConsoleInput "x")]])
        it "for x=1 to 2{>x}<x" $
            parse parseProgram "fileName" "for x=1 to 2{>x}<x" `shouldBe` Right ([For "x" (Lit 1) (Lit 2) [VarExpr (ConsoleInput "x")], VarExpr (ConsoleOutput (Var "x"))])
        it "forx x=1 to 2{>x}" $
            parse parseProgram "fileName" "forx x=1 to 2{>x}" `shouldSatisfy` isLeft
    describe "Parse then run program" $ do
        it "mut x = 5\\n  mut x = 10" $ do
            let result = parse parseProgram "fileName" "mut x = 5\n  mut x = 10"
            case result of
                (Right st) -> do
                    r <- runExceptT (runStateT (runProgram st) (StateEnvironment Map.empty))
                    r `shouldBe` Left (VarError (NameAlreadyExistsState "x"))
        it "mut x = 5\\n x = 10" $ do
            let result = parse parseProgram "fileName" "mut x = 5\n x = 10"
            case result of
                (Right st) -> do
                    r <- runExceptT (runStateT (runProgram st) (StateEnvironment Map.empty))
                    s <- runExceptT (execStateT (runProgram st) (StateEnvironment Map.empty))
                    r `shouldSatisfy` isRight
                    case s of 
                        Right env -> env `shouldBe` StateEnvironment (Map.fromList [("x", 10)])
        it "x = 10" $ do
            let result = parse parseProgram "fileName" "x = 10"
            case result of
                (Right st) -> do
                    r <- runExceptT (runStateT (runProgram st) (StateEnvironment Map.empty))
                    r `shouldBe` Left (VarError (NameNotFoundState "x"))
        it "mut x = 10 x = x / 10" $ do
            let result = parse parseProgram "fileName" "mut x = 10 x = x / 10"
            case result of
                (Right st) -> do
                    r <- runExceptT (runStateT (runProgram st) (StateEnvironment Map.empty))
                    s <- runExceptT (execStateT (runProgram st) (StateEnvironment Map.empty))
                    r `shouldSatisfy` isRight
                    case s of 
                        Right env -> env `shouldBe` StateEnvironment (Map.fromList [("x", 1)])
        it "mut x = 10 x = x / 0" $ do
            let result = parse parseProgram "fileName" "mut x = 10 x = x / 0"
            case result of
                (Right st) -> do
                    r <- runExceptT (runStateT (runProgram st) (StateEnvironment Map.empty))
                    r `shouldBe` Left (ArithmError DivisionByZero)
        it "factorial 4" $ do 
            let result = parse parseProgram "fileName" fact
            case result of
                (Right st) -> do
                    r <- runExceptT (runStateT (runProgram st) (StateEnvironment Map.empty))
                    s <- runExceptT (execStateT (runProgram st) (StateEnvironment Map.empty))
                    r `shouldSatisfy` isRight
                    case s of 
                        Right env -> env `shouldBe` StateEnvironment (Map.fromList [("x", 24)])
        it "counter name test" $ do 
            let result = parse parseProgram "fileName" counterNameTest
            case result of
                (Right st) -> do
                    r <- runExceptT (runStateT (runProgram st) (StateEnvironment Map.empty))
                    r `shouldBe` Left (ForError CounterNameAlreadyExists)