module TestSpec (spec) where

import           Test.Hspec
import qualified Data.List (sortBy, permutations)
import           Data.Function (on)
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

import           Lib2

-- x = Partial (\l -> Just $ l + 1)
-- y = Partial (\l -> Just $ l * 3)
-- z = Partial (\_ -> Nothing)
-- test = x Control.Category.. y Control.Category.. z

list1 = Data.List.sortBy compare [[0, 0], [0, 1], [1, 0], [1, 1]]
list2 = Data.List.sortBy compare [ [ 0 , 0 , 0 ]
 , [ 1 , 0 , 0 ]
 , [ 0 , 1 , 0 ]
 , [ 1 , 1 , 0 ]
 , [ 0 , 0 , 1 ]
 , [ 1 , 0 , 1 ]
 , [ 0 , 1 , 1 ]
 , [ 1 , 1 , 1 ]]

combinations1 = Data.List.sortBy compare [ [1, 2]
 , [1, 3]
 , [1, 4]
 , [2, 3]
 , [2, 4]
 , [3, 4]
 ]

prop_permutations :: Int -> Bool
prop_permutations len = let expected = Data.List.sortBy compare $ Data.List.permutations [1..len] in
    (Data.List.sortBy compare $ permutations [1..len]) == expected

spec :: Spec
spec = do
    describe "Block1" $ do
        it "eval" $ do
            eval (Const 5) `shouldBe` Right 5
            eval (Div (Const 5) (Const 0)) `shouldBe` Left "Dividing by zero"
            eval (Pow (Const 5) (Const 0)) `shouldBe` Right 1
            eval (Pow (Const 5) (Const (-1))) `shouldBe` Left "Raising to the negative power"
    describe "Block1" $ do
        it "bin" $ do
            bin (-1) `shouldBe` Nothing
            bin 0 `shouldBe` Just [[]]
            bin 1 `shouldBe` Just [[0],[1]]
            bin 2 `shouldBe` Just list1
            bin 3 `shouldBe` Just list2
        it "combinations" $ do
            combinations (-1) (-2) `shouldBe` Nothing
            combinations 0 0 `shouldBe` Nothing
            combinations 4 2 `shouldBe` Just combinations1
        it "permutations" $ prop_permutations 6