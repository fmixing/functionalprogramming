module TestSpec (spec) where

import           Data.Foldable         (toList)
import           Data.List             (nub, sort)
import           Data.Maybe            (isJust, isNothing)
import           Data.Monoid           (Any(..), Sum(..)) 
import           Data.Monoid           hiding ((<>), Endo)
import           Data.Semigroup as Semigroup ((<>))
import           Test.Hspec

import           Lib

passTests = [ "1", "1 2 3", " 1", "1 ", "\t1\t", "\t12345\t", "010 020 030" ,
 " 123 456 789 ", "-1", "-1 -2 -3", "\t-12345\t", " -123 -456 -789 ",
  "\n1\t\n3   555  -1\n\n\n-5", "123\t\n\t\n\t\n321 -4 -40"]

mustFail  = ["asd", "1-1", "1.2", "--2", "+1", "1+"]

advancedTests    = [ "+1", "1 +1", "-1 +1", "+1 -1"]

advancedMustFail = ["1+1", "++1", "-+1", "+-1", "1 + 1", "1 ++ 1"]

fighting1 = "The fight has begun\nRound 1: The fighter Knight {hp = 10, attack = 5} attacked his opponent\n"
    ++ "Round 2: The fighter Monster {hp = 5, attack = 5} striked his opponent back\n"
    ++ "Round 3: The fighter Knight {hp = 5, attack = 5} win!\n"

fighting2 = "The fight has begun\nRound 1: The fighter Knight {hp = 10, attack = 5} attacked his opponent\n"
    ++ "Round 2: The fighter Knight {hp = 5, attack = 5} striked his opponent back\n"
    ++ "Round 3: The fighter Knight {hp = 5, attack = 5} win!\n"

fighting3 = "The fight has begun\nRound 1: The fighter Monster {hp = 10, attack = 5} attacked his opponent\n"
    ++ "Round 2: The fighter Monster {hp = 5, attack = 5} striked his opponent back\n"
    ++ "Round 3: The fighter Monster {hp = 5, attack = 5} win!\n"

arrToSort = [38,-53,-21,98,-92,-69,-52,1,-89]

tree = (Node (3 :: Int) (Node (1 :: Int) Leaf Leaf) (Node (123 :: Int) (Node (4 :: Int) Leaf Leaf) Leaf))
treeForMonoidTest = toList $ tree `mappend` (Node 2 Leaf Leaf)

treeFromList = fromList arrToSort

endo1 = Endo (\l -> l * 2)
endo2 = Endo (\l -> l + 1)
endo3 = Endo (\l -> l + 3)
endo4 = (mempty :: Endo Int)

fun1 = getEndo ((endo1 <> endo2) <> endo3)
fun2 = getEndo (endo1 <> (endo2 <> endo3))
fun3 = getEndo (endo1 <> endo4)

arrow1 = Arrow (\l -> [[l, l]])
arrow2 = Arrow (\l -> [[l], [l]])
arrow3 = mempty

funArr1 = getArrow (arrow1 `mappend` arrow2)
funArr2 = getArrow (arrow1 `mappend` arrow3)

spec :: Spec
spec = do
    describe "Block1" $ do
        it "order3" $ do
            order3 (3, 2, 1) `shouldBe` (1, 2, 3)
            order3 (2, 3, 1) `shouldBe` (1, 2, 3)
            order3 (1, 2, 3) `shouldBe` (1, 2, 3)
        it "highestBit" $ do
            highestBit 0 `shouldBe` Nothing
            highestBit 1 `shouldBe` Just 1
            highestBit 15 `shouldBe` Just 8
            highestBit 16 `shouldBe` Just 16
        it "highestBitHard" $ do
            highestBitHard 0 `shouldBe` Nothing
            highestBitHard 1 `shouldBe` Just (1, 0)
            highestBitHard 15 `shouldBe` Just (8, 3)
            highestBitHard 16 `shouldBe` Just (16, 4)
        it "smartReplicate" $ do
            smartReplicate [1, 2, 3] `shouldBe` [1, 2, 2, 3, 3, 3]
            smartReplicate [1, 2, 0] `shouldBe` [1, 2, 2]
        it "contains" $ do
            contains 3 [[1..5], [2,0], [3,4]] `shouldBe` [[1,2,3,4,5],[3,4]]
            contains 10 [[1..5], [2,0], [3,4]] `shouldBe` []
    describe "Block2" $ do
        it "removeAt" $ do
            removeAt 1 [1,2,3] `shouldBe` Just [1, 3]
            removeAt 10 [1,2,3] `shouldBe` Just [1,2,3]
            removeAt 3 [1..5] `shouldBe` Just [1,2,3,5]
            removeAt 2 "abc" `shouldBe` Just "ab"
            removeAt (-1) "abc" `shouldBe` Nothing
        it "removeAtHard" $ do
            removeAtHard 1 [1,2,3] `shouldBe` (Just 2, [1, 3])
            removeAtHard 10 [1,2,3] `shouldBe` (Nothing, [1, 2, 3])
            removeAtHard (-1) [1,2,3] `shouldBe` (Nothing, [1, 2, 3])
        it "collectEvery" $ do
            collectEvery 3 [1..8] `shouldBe` Just ([1,2,4,5,7,8], [3,6])
            collectEvery 10 [1..8] `shouldBe` Just ([1,2,3,4,5,6,7,8], [])
            collectEvery 0 [1..8] `shouldBe` Nothing
        it "stringSum" $ do
            map stringSum passTests `shouldBe` [Just 1, Just 6, Just 1, Just 1, Just 1, Just 12345,
                Just 60, Just 1368, Just (-1), Just (-6), Just (-12345), Just (-1368), Just 553, Just 400]
            map stringSum mustFail `shouldBe` [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
        it "stringSumHard" $ do
            map stringSumHard advancedTests `shouldBe` [Just 1, Just 2, Just 0, Just 0]
            map stringSumHard advancedMustFail`shouldBe` [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
        it "mergeSort" $ do
            mergeSort arrToSort `shouldBe` sort arrToSort 
    describe "Block3" $ do
        it "nextDay" $ do
            nextDay Monday `shouldBe` Tuesday
            nextDay Sunday `shouldBe` Monday
        it "afterDays" $ do
            afterDays Monday 5 `shouldBe` Saturday
            afterDays Saturday 5 `shouldBe` Thursday
        it "isWeekend" $ do
            isWeekend Saturday `shouldBe` True
        it "daysToParty" $ do 
            daysToParty Friday `shouldBe` 0
            daysToParty Sunday `shouldBe` 5
        it "fighting" $ do
            fighting (Knight 10 5) (Monster 10 5) `shouldBe` fighting1
            fighting (Knight 10 5) (Knight 10 5) `shouldBe` fighting2
            fighting (Monster 10 5) (Monster 10 5) `shouldBe` fighting3
        it "vecLength" $ do
            vecLength (Vector3D 0.0 0.0 0.0) `shouldBe` 0
            vecLength (Vector3D 1.0 2.0 3.0) `shouldBe` 3.7416573867739413
            vecLength (Vector2D 5.0 12.0) `shouldBe` 13
        it "vecLength" $ do
            vecSum (Vector3D 1.0 2.0 3.0) (Vector3D 4 5 6) `shouldBe` (Vector3D 5 7 9)
            vecSum (Vector2D 1 2) (Vector3D 4 5 6) `shouldBe` (Vector3D 5 7 6)
        it "vecMul" $ do 
            vecMul (Vector3D 1 2 3) (Vector3D 4 5 6) `shouldBe` 32
            vecMul (Vector2D 1 2) (Vector3D 4 5 6) `shouldBe` 14
        it "dist" $ do 
            dist (Vector3D 0 0 0) (Vector2D 12 5) `shouldBe` 13
        it "vecMulVect" $ do 
            vecMulVect (Vector3D 1 2 3) (Vector3D 4 5 6) `shouldBe` (Vector3D (-3) 6 (-3))
            vecMulVect (Vector3D 1 2 3) (Vector3D 2 4 6) `shouldBe` (Vector3D 0 0 0)
        it "natSum" $ do 
            natSum (S (S Z)) (S (S (S Z))) `shouldBe` (S (S (S (S (S Z)))))
            natSum (S (S Z)) (Z) `shouldBe` (S (S Z))
        it "natMul" $ do
            natMul (S (S Z)) (S (S (S Z))) `shouldBe` (S (S (S (S (S (S Z))))))
            natMul (S (S Z)) (Z) `shouldBe` (Z)
        it "natSub" $ do
            natSub (S (S Z)) (S (S (S Z))) `shouldBe` Nothing
            natSub (S (S Z)) (Z) `shouldBe` Just (S (S Z))
            natSub (S (S (S Z))) (S (S Z)) `shouldBe` Just (S Z)
        it "natToNum" $ do
            natToNum (S (S Z)) `shouldBe` 2
            natToNum (Z) `shouldBe` 0
        it "equality" $ do
            (S (S (S (S Z)))) == (S (S (S Z))) `shouldBe` False
            (S (S (S (S Z)))) == (S (S (S (S Z)))) `shouldBe` True
        it "ordering" $ do
            (S (S (S (S Z)))) < (S (S (S Z))) `shouldBe` False
            (S (S (S (S Z)))) > (S (S (S Z))) `shouldBe` True
            (S (S (S (S Z)))) > (S (S (S (S Z)))) `shouldBe` False
        it "isEven" $ do
            isEven (S (S (S (S Z)))) `shouldBe` True
            isEven (S (S (S Z))) `shouldBe` False
            isEven (Z) `shouldBe` True
        it "natDiv" $ do
            natDiv (S (S Z)) (S (S (S Z))) `shouldBe` Just (Z)
            natDiv (S (S (S Z))) (S (S Z)) `shouldBe` Just (S (Z))
            natDiv (S (S Z)) (Z) `shouldBe` Nothing
            natDiv (S (S (S (S (S (S Z)))))) (S (S (S Z))) `shouldBe` Just (S (S Z))
        it "natMod" $ do
            natMod (S (S Z)) (S (S (S Z))) `shouldBe` Just (S (S Z))
            natMod (S (S (S Z))) (S (S Z)) `shouldBe` Just (S (Z))
            natMod (S (S Z)) (Z) `shouldBe` Nothing
            natMod (S (S (S (S (S (S Z)))))) (S (S (S Z))) `shouldBe` Just (Z)
        it "natGcd" $ do
            natGcd (Z) (S (S Z)) `shouldBe` Nothing
            natGcd (S (S Z)) (Z) `shouldBe` Nothing
            natGcd (S (S (S (S (S (S Z)))))) (S (S (S Z))) `shouldBe` Just (S (S (S Z))) 
        it "isEmptyTree" $ do
            isEmptyTree (Leaf) `shouldBe` True
            isEmptyTree tree `shouldBe` False
        it "findElemTree" $ do
            findElemTree (Leaf) 1 `shouldBe` False
            findElemTree tree 1 `shouldBe` True
            findElemTree tree 2 `shouldBe` False
        it "sizeTree" $ do
            sizeTree (Leaf) `shouldBe` 0
            sizeTree tree `shouldBe` 4
        it "insertIntoTree" $ do
            insertIntoTree (Leaf) 2 `shouldBe` (Node (2::Int) Leaf Leaf)
            insertIntoTree tree 2 `shouldBe` (Node (3::Int) (Node (1::Int) Leaf (Node (2::Int) Leaf Leaf)) $ Node (123::Int) (Node (4::Int) Leaf Leaf) Leaf)
            insertIntoTree tree 123 `shouldBe` tree
        it "fromList" $ do
            fromList ([] :: [Int]) `shouldBe` Leaf
            fromList [1, 2, 3, 4, 5, 6, 7] `shouldBe` (Node 7 (Node 6 (Node 5 (Node 4 (Node 3 (Node 2 (Node 1 Leaf Leaf) Leaf) Leaf) Leaf) Leaf) Leaf) Leaf)
    describe "Block4" $ do
        it "foldMapTree" $ do 
            foldMap (Any . (== 1)) tree `shouldBe` (Any {getAny = True})
            foldMap (Any . (== 2)) tree `shouldBe` (Any {getAny = False})
        it "foldrTree" $ do
            toList treeFromList `shouldBe` sort arrToSort
        it "splitOn" $ do 
            splitOn '/' "path/to/file" `shouldBe` ["path", "to", "file"]
            splitOn '/' "//" `shouldBe` ["", "", ""]
        it "joinWith" $ do 
            joinWith '/' ["path", "to", "file"] `shouldBe` "path/to/file"
            joinWith '/' ["", "", "", ""] `shouldBe` "///"
    describe "Block5" $ do 
        it "maybeConcat" $ do
            maybeConcat [Just [1,2,3], Nothing, Just [4,5]] `shouldBe` [1, 2, 3, 4, 5]
            maybeConcat ([Nothing] :: [Maybe [Int]]) `shouldBe` ([] :: [Int]) 
        it "eitherConcat" $ do
            eitherConcat [Left (Sum 3), Right [1,2,3], Left (Sum 5), Right [4,5]] `shouldBe` Just (Sum {getSum = 8}, [1,2,3,4,5])
            eitherConcat ([Left (Sum 3), Left (Sum 5)] :: [Either (Sum Integer) [Int]]) `shouldBe` Nothing
        it "Semigroup.NonEmpty" $ do 
            ((1 :| [2, 3, 4]) :: NonEmpty Int) <> ((5 :| [6, 7, 8]) :: NonEmpty Int) `shouldBe` ((1 :| [2, 3, 4, 5, 6, 7, 8]) :: NonEmpty Int)
        it "Monoid.Identity" $ do 
            (Identity [1]) `mappend` (Identity [2]) `shouldBe` (Identity [1, 2])
            (Identity [1]) `mappend` (Identity ([] :: [Int])) `shouldBe` (Identity [1])
        it "Monoid.Name" $ do 
            (Name "root") <> (Name "server") `shouldBe` (Name "root.server")
            (Name "root") `mappend` (Name "") `shouldBe` (Name "root")
            (Name "") `mappend` (Name "server") `shouldBe` (Name "server")
        it "Monoid.Endo" $ do 
            fun1 1 `shouldBe` 10
            fun2 1 `shouldBe` 10
            fun3 1 `shouldBe` 2
        it "Monoid.Arrow" $ do
            funArr1 1 `shouldBe` [[1, 1], [1], [1]]
            funArr2 1 `shouldBe` [[1, 1]]
        it "MonoidTree" $ do 
            tree <> tree `shouldBe` tree
            Leaf `mappend` tree `shouldBe` tree
            treeForMonoidTest `shouldBe` [1, 2, 3, 4, 123]