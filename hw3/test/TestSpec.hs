module TestSpec (spec) where

import           Test.Hspec
import qualified Data.List (sortBy, permutations)
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Data.Char

import qualified Data.Map.Strict as Map (fromList)
import           Lib3
import Control.Monad.Reader (runReader)
import Text.Megaparsec (parse)
import Text.Megaparsec.Error (ParseError (..))
import Data.Either (isLeft)


evaluateRight = Var "x" `Add` (Lit 3 `Mul` ("x" `Let` (Div (Lit 5) (Lit 2)) $ Var "x"))
evaluateWrong = Var "x" `Add` (Lit 3 `Mul` ("x" `Let` (Div (Lit 5) (Lit 0)) $ Var "x"))

parseRight = ABinary AAdd (AVar "x") (ABinary AMul (ALit 3) (ALet "x" (ABinary AMul (AVar "y") (ALit 3)) (ABinary ADiv (AVar "x") (ALit 2))))

spec :: Spec
spec = do
    describe "ArithmExpr" $ do
        it "evaluate" $ do
            runReader (evaluate evaluateRight) (Environment $ Map.fromList [("x", 1)]) `shouldBe` Right 7
            runReader (evaluate evaluateWrong) (Environment $ Map.fromList [("x", 1)]) `shouldBe` Left DivisionByZero
            runReader (evaluate evaluateRight) (Environment $ Map.fromList [("y", 1)]) `shouldBe` Left NameNotFound
    describe "Parse" $ do
        it "whileParser" $ do
            parse whileParser "fileName" "x + 3 * (let x = y * 3 in x / 2)" `shouldBe` Right parseRight
            parse whileParser "fileName" "let x = 2 in x" `shouldSatisfy` isLeft
            parse whileParser "fileName" "(let x = 2 in x" `shouldSatisfy` isLeft