module TestSpec (spec) where

import           Test.Hspec
import qualified Data.List (sortBy, permutations)
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Data.Char

import           Lib2

-- x = Partial (\l -> Just $ l + 1)
-- y = Partial (\l -> Just $ l * 3)
-- z = Partial (\_ -> Nothing)
-- test = x Control.Category.. y Control.Category.. z
-- oE = orElse z x  // apply oE 0 -> Just 1
-- wD = withDefault z 1 // apply wD 2 -> Just 1 // wD1 = withDefault wD 2 // apply wD1 3 -> Just 2

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

personParser = Emp <$> parseName <*> parsePhone

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
    describe "Block2" $ do
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
    describe "Block4" $ do
        it "personParser" $ do
            runParser personParser "Alice123" `shouldBe` Just (Emp {name = "Alice", phone = "123"},"")
        it "abParser" $ do
            runParser abParser "abc" `shouldBe` Just (('a', 'b'), "c")
            runParser abParser "bc" `shouldBe` Nothing
            runParser abParser "ac" `shouldBe` Nothing
        it "abParser_" $ do
            runParser abParser_ "abc" `shouldBe` Just ((), "c")
            runParser abParser_ "bc" `shouldBe` Nothing
            runParser abParser_ "ac" `shouldBe` Nothing
        it "intPair" $ do
            runParser intPair "12 34" `shouldBe` Just ([12,34],"")
            runParser intPair "12  34" `shouldBe` Nothing
            runParser intPair "1234" `shouldBe` Nothing
        it "intOrUppercase" $ do
            runParser intOrUppercase "123abc" `shouldBe` Just ((), "abc")
            runParser intOrUppercase "ABC" `shouldBe` Just ((), "BC")
            runParser intOrUppercase "abc" `shouldBe` Nothing
        it "zeroOrMore" $ do 
            runParser (zeroOrMore $ satisfy (Data.Char.isUpper)) "ABCc" `shouldBe` Just ("ABC","c")
            runParser (zeroOrMore $ satisfy (Data.Char.isUpper)) "c" `shouldBe` Just ("","c")
        it "oneOrMore" $ do 
            runParser (oneOrMore $ satisfy (Data.Char.isUpper)) "ABCc" `shouldBe` Just ("ABC","c")
            runParser (oneOrMore $ satisfy (Data.Char.isUpper)) "c" `shouldBe` Nothing
        it "spaces" $ do
            runParser spaces "   " `shouldBe` Just ("   ","")
            runParser spaces "123" `shouldBe` Just ("","123")
        it "ident" $ do 
            runParser ident "foobar baz" `shouldBe` Just ("foobar"," baz")
            runParser ident "foo33fA" `shouldBe` Just ("foo33fA","")
            runParser ident "2bad" `shouldBe` Nothing
            runParser ident " " `shouldBe` Nothing
        it "parseSExpr" $ do 
            runParser parseSExpr "123" `shouldBe` Just (A (N 123),"")
            runParser parseSExpr "a123" `shouldBe` Just (A (I "a123"),"")
            runParser parseSExpr "(bar (foo) 3 5 874)" `shouldBe` 
                Just (Comb [A (I "bar"),Comb [A (I "foo")],A (N 3),A (N 5),A (N 874)],"")
            runParser parseSExpr "(((lambda x (lambda y (plus x y))) 3) 5)" `shouldBe` 
                Just (Comb [Comb [Comb [A (I "lambda"),A (I "x"),Comb [A (I "lambda"),A (I "y"),
                Comb [A (I "plus"),A (I "x"),A (I "y")]]],A (N 3)],A (N 5)],"")
            runParser parseSExpr "(   lots  of   (  spaces   in  )  this ( one ) )" `shouldBe`
                Just (Comb [A (I "lots"),A (I "of"),Comb [A (I "spaces"),A (I "in")],A (I "this"),Comb [A (I "one")]],"")
            