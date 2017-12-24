{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE RankNTypes #-}

module TestSpec (spec) where

import           Test.Hspec
import qualified Data.List (sortBy, permutations)
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

import           Lib4

x = (1, 2)
k = (1, (2, 7))
-- _11 = (lens fst (\(x1, x2) y -> (y, x2)))
-- _22 = (lens snd (\(x1, x2) y -> (x1, y)))
-- newL = choosing _1 _2

s1 = set (choosing _1 _2) 10  (Left x) :: Either (Double, Double) (Double, Double)
s2 = set (choosing _1 _2) 10 (Right x) :: Either (Double, Double) (Double, Double)
o1 = over (choosing _1 _2) (\x' -> x' ** 7) (Right x) :: (Either (Double, Double) (Double, Double))
o2 = over (choosing _1 _2) (\x -> x ** 7) (Left x) :: (Either (Double, Double) (Double, Double))

spec :: Spec
spec = do
    describe "Pair Lens" $ do
        it "view test" $ do
            view _1 x `shouldBe` 1
            view _2 x `shouldBe` 2
            ((_2 `view`) . (_2 `view`)) k `shouldBe` 7
            ((_1 `view`) . (_2 `view`)) k `shouldBe` 2
        it "set test" $ do
            set _1 3 x `shouldBe` (3, 2)
            set _2 10 x `shouldBe` (1, 10)
        it "over test" $ do
            over _1 (\y -> y * 5) x`shouldBe` (5, 2)
            over _2 (\y -> y * 5) x `shouldBe` (1, 10)
            over _2 (\y -> set _1 3 y) k `shouldBe` (1, (3, 7))
            over _2 (\y -> set _2 3 y) k `shouldBe` (1, (2, 3))
    describe "Advanced Pair Lens" $ do
        it "view test" $ do
            view (lens fst (\(x1, x2) y -> (y, x2))) x `shouldBe` 1
            view (lens snd (\(x1, x2) y -> (x1, y))) x `shouldBe` 2
        it "set test" $ do
            set (lens fst (\(x1, x2) y -> (y, x2))) 3 x `shouldBe` (3, 2)
            set (lens snd (\(x1, x2) y -> (x1, y))) 3 x `shouldBe` (1, 3)
        it "over test" $ do
            over (lens fst (\(x1, x2) y -> (y, x2))) (\y -> y * 5) x `shouldBe` (5, 2)
            over (lens snd (\(x1, x2) y -> (x1, y))) (\y -> y * 5) x `shouldBe` (1, 10)
        it "choosing test view" $ do
            view (choosing _1 _2) (Right x) `shouldBe` 2
            view (choosing _1 _2) (Left x) `shouldBe` 1
        it "choosing test set" $ do
            s1 `shouldBe` Left (10, 2)
            s2 `shouldBe` Right (1, 10)
        it "choosing test over" $ do
            o1 `shouldBe` Right (1.0,128.0)
            o2 `shouldBe` Left (1.0,2)
        it "test <%~" $ do
            (<%~) _1 (\x -> x + 3) x `shouldBe` (4,(4,2))
            (<%~) _2 (\x -> x + 3) x `shouldBe` (5,(1,5))
        it "test <<%~" $ do
            (<<%~) _2 (\x -> x + 3) x `shouldBe` (2,(1,5))
            (<<%~) _1 (\x -> x + 3) x `shouldBe` (1,(4,2))