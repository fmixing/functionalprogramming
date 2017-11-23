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
import Data.Maybe (isNothing, isJust)
import Control.Monad.State (State, get, modify, state, runState)


evaluateRight = Var "x" `Add` (Lit 3 `Mul` ("x" `Let` (Div (Lit 5) (Lit 2)) $ Var "x"))
evaluateWrong = Var "x" `Add` (Lit 3 `Mul` ("x" `Let` (Div (Lit 5) (Lit 0)) $ Var "x"))

parseRight = ABinary AAdd (AVar "x") (ABinary AMul (ALit 3) (ALet "x" (ABinary AMul (AVar "y") (ALit 3)) (ABinary ADiv (AVar "x") (ALit 2))))

dec = Declare "x" (ALit 1)
upd = Update "x" (ALet "x" (ALit 2) (AVar "x"))
bad = Declare "x" (ABinary AAdd (AVar "x") (ABinary ADiv (ABinary AMul (ALit 3) (ALit 2)) (ALit 0)))

prop_declare :: Bool
prop_declare = ans1 && ans2
  where
    ans1 = let ans = runState (declare "y" 1) (StateEnvironment $ Map.fromList [("x", 1)]) in isNothing $ fst ans 
    ans2 = let ans = runState (declare "y" 1) (StateEnvironment $ Map.fromList [("y", 1)]) in isJust $ fst ans
    
prop_parse_var :: Bool
prop_parse_var = ans1 && ans2 && ans3 && ans4
  where
    ans1 = let ans = runState (evaluateVarExpr [dec, upd]) (StateEnvironment $ Map.empty) in isNothing $ fst ans 
    ans2 = let ans = runState (evaluateVarExpr [upd, dec]) (StateEnvironment $ Map.empty) in isJust $ fst ans
    ans3 = let ans = runState (evaluateVarExpr [bad, upd]) (StateEnvironment $ Map.empty) in isJust $ fst ans
    ans4 = let ans = runState (evaluateVarExpr [dec, dec]) (StateEnvironment $ Map.empty) in isJust $ fst ans
    
spec :: Spec
spec = do
    describe "ArithmExpr" $ do
        it "evaluate" $ do
            runReader (evaluate evaluateRight) (Environment $ Map.fromList [("x", 1)]) `shouldBe` Right 7
            runReader (evaluate evaluateWrong) (Environment $ Map.fromList [("x", 1)]) `shouldBe` Left (ArithmError DivisionByZero)
            runReader (evaluate evaluateRight) (Environment $ Map.fromList [("y", 1)]) `shouldBe` Left (ArithmError NameNotFound)
            runReader (evaluate evaluateRight) (Environment $ Map.fromList []) `shouldBe` Left (ArithmError NameNotFound)
    describe "Parse" $ do
        it "whileParser" $ do
            parse whileParser "fileName" "x + 3 * (let x = y * 3 in x / 2)" `shouldBe` Right parseRight
            parse whileParser "fileName" "let x = 2 in x" `shouldSatisfy` isLeft
            parse whileParser "fileName" "(let x = 2 in x" `shouldSatisfy` isLeft
    describe "Declare and update" $ do
        it "declare" $ prop_declare
    describe "Parse Var" $ do
        it "parseVarExpr" $ do
            parse parseVarExpr "fileName" "mut x = 1" `shouldBe` Right (Declare "x" (ALit 1))
    describe "blabla" $ do
        it "eval" $ prop_parse_var
        -- it "eval" $ do
        --     runState (evaluateVarExpr [dec, upd]) (StateEnvironment $ Map.empty) `shouldSatisfy` isNothing
        --     runState (evaluateVarExpr [upd, dec]) (StateEnvironment $ Map.empty) `shouldSatisfy` isJust
        --     runState (evaluateVarExpr [bad, upd]) (StateEnvironment $ Map.empty) `shouldSatisfy` isJust
        --     runState (evaluateVarExpr [dec, dec]) (StateEnvironment $ Map.empty) `shouldSatisfy` isJust