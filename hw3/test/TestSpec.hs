module TestSpec (spec) where

import           Test.Hspec
import qualified Data.List (sortBy, permutations)
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Data.Char

import qualified Data.Map.Strict as Map (Map, fromList, empty)
import           Lib3
import Control.Monad.Reader (runReader)
import Text.Megaparsec (parse)
import Text.Megaparsec.Error (ParseError (..))
import Data.Either (isLeft)
import Data.Maybe (isNothing, isJust, fromJust)
import Control.Monad.State (State, get, modify, state, runState, evalStateT, execStateT, runStateT)


evaluateRight = Var "x" `Add` (Lit 3 `Mul` ("x" `Let` (Div (Lit 5) (Lit 2)) $ Var "x"))
evaluateWrong = Var "x" `Add` (Lit 3 `Mul` ("x" `Let` (Div (Lit 5) (Lit 0)) $ Var "x"))

parseRight = Add (Var "x") (Mul (Lit 3) (Let "x" (Mul (Var "y") (Lit 3)) (Div (Var "x") (Lit 2))))

fact = "mut x = 1\nfor i = 1 to 5 {\n    x = x * i\n}"
counterNameTest = "mut x = 1\nfor x = 1 to 5 {\n    x = x * x\n}"
    
spec :: Spec
spec = do
    describe "Evaluate ArithmExpr" $ do
        it "x + 3 * (let x = 5 / 2 in x) with (Env x = 1) -> 7" $ do
            runReader (evaluate evaluateRight) (Environment $ Map.fromList [("x", 1)]) `shouldBe` Right 7
        it "x + 3 * (let x = 5 / 0 in x) with (Env x = 1) -> DivisionByZero" $ do
            runReader (evaluate evaluateWrong) (Environment $ Map.fromList [("x", 1)]) `shouldBe` Left (ArithmError DivisionByZero)
        it "x + 3 * (let x = 5 / 0 in x) with (Env y = 1) -> NameNotFound" $ do
            runReader (evaluate evaluateRight) (Environment $ Map.fromList [("y", 1)]) `shouldBe` Left (ArithmError (NameNotFound "x"))
        it "x + 3 * (let x = 5 / 0 in x) with (Env is empty) -> NameNotFound" $ do
            runReader (evaluate evaluateRight) (Environment $ Map.fromList []) `shouldBe` Left (ArithmError (NameNotFound "x"))
        it "2 / x with (Env x = 0) -> DivisionByZero" $ do
            runReader (evaluate (Div (Lit 2) (Var "x"))) (Environment $ Map.fromList [("x", 0)]) `shouldBe` Left (ArithmError DivisionByZero)
        it "2 / x with (Env x = 1) -> 2" $ do
            runReader (evaluate (Div (Lit 2) (Var "x"))) (Environment $ Map.fromList [("x", 1)]) `shouldBe` Right 2
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
                (Right r) -> runReader (evaluate r) (Environment $ Map.fromList []) `shouldBe` Right 10
        it "((((x + 10))) * (((x - 10)))) + ((let x = 0 in x + x))" $
            case parse whileParser "fileName" "((((x + 10))) * (((x - 10)))) + ((let x = 0 in x + x))" of
                (Right r) -> runReader (evaluate r) (Environment $ Map.fromList [("x", 11)]) `shouldBe` Right 21
        it "(2 * 10) + (let x = 10 in (x * 4)) / 0" $
            case parse whileParser "fileName" "(((2 * 10))) + (let x = 10 in (x * 4)) / 0" of
                (Right r) -> runReader (evaluate r) (Environment $ Map.fromList [("x", 11)]) `shouldBe` Left (ArithmError DivisionByZero)
    describe "Declare and update" $ do
        it "declare y = 1 with (Env x = 1) -> Nothing" $ 
            fromJust (evalStateT (declare "y" 1) (StateEnvironment $ Map.fromList [("x", 1)])) `shouldBe` Nothing
        it "declare y = 1 with (Env y = 1) -> NameAlreadyExistsState" $ 
            fromJust (evalStateT (declare "y" 1) (StateEnvironment $ Map.fromList [("y", 1)])) `shouldBe` Just (VarError (NameAlreadyExistsState "y"))
        it "update y = 2 with (Env y = 1) -> Nothing" $ 
            fromJust (evalStateT (update "y" 2) (StateEnvironment $ Map.fromList [("y", 1)])) `shouldBe` Nothing
        it "update y = 2 with (Env x = 1) -> NameNotFoundState" $ 
            fromJust (evalStateT (update "y" 2) (StateEnvironment $ Map.fromList [("x", 1)])) `shouldBe` Just (VarError (NameNotFoundState "y"))
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
                    r <- runStateT (runProgram st) (StateEnvironment Map.empty)
                    r `shouldBe` (Just (VarError (NameAlreadyExistsState "x")), StateEnvironment (Map.fromList [("x", 5)]))
        it "mut x = 5\\n x = 10" $ do
            let result = parse parseProgram "fileName" "mut x = 5\n x = 10"
            case result of
                (Right st) -> do
                    r <- runStateT (runProgram st) (StateEnvironment Map.empty)
                    r `shouldBe` (Nothing, StateEnvironment (Map.fromList [("x", 10)]))
        it "x = 10" $ do
            let result = parse parseProgram "fileName" "x = 10"
            case result of
                (Right st) -> do
                    r <- runStateT (runProgram st) (StateEnvironment Map.empty)
                    r `shouldBe` (Just (VarError (NameNotFoundState "x")), StateEnvironment (Map.fromList []))
        it "mut x = 10 x = x / 10" $ do
            let result = parse parseProgram "fileName" "mut x = 10 x = x / 10"
            case result of
                (Right st) -> do
                    r <- runStateT (runProgram st) (StateEnvironment Map.empty)
                    r `shouldBe` (Nothing, StateEnvironment (Map.fromList [("x", 1)]))
        it "mut x = 10 x = x / 0" $ do
            let result = parse parseProgram "fileName" "mut x = 10 x = x / 0"
            case result of
                (Right st) -> do
                    r <- runStateT (runProgram st) (StateEnvironment Map.empty)
                    r `shouldBe` (Just (ArithmError DivisionByZero), StateEnvironment (Map.fromList [("x", 10)]))
        it "factorial 4" $ do 
            let result = parse parseProgram "fileName" fact
            case result of
                (Right st) -> do
                    r <- runStateT (runProgram st) (StateEnvironment Map.empty)
                    r `shouldBe` (Nothing, StateEnvironment (Map.fromList [("x", 24)]))
        it "counter name test" $ do 
            let result = parse parseProgram "fileName" counterNameTest
            case result of
                (Right st) -> do
                    r <- runStateT (runProgram st) (StateEnvironment Map.empty)
                    r `shouldBe` (Just (ForError CounterNameAlreadyExists), StateEnvironment (Map.fromList [("x", 1)]))