{-# LANGUAGE OverloadedStrings          #-}

module TestSpec (spec) where

import           Test.Hspec
import qualified Data.List (sortBy, permutations)
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

import           Lib4

    
spec :: Spec
spec = do
    describe "Pair Lens" $ do
        it "" $ do
            -- x = (1, 2)
            -- view _1 x -> 1
            -- view _2 x -> 2
            -- set _1 3 x -> (3, 2)
            -- set _2 10 x -> (1, 10)
            -- over _2 (\y -> y * 5) x -> (1, 10)
            -- over _1 (\y -> y * 5) x -> (5, 2)
            -- _11 = lens fst (\(x1, x2) y -> (y, x2))
            -- _22 = lens snd (\(x1, x2) y -> (x1, y))
            -- set _11 3 x -> (3, 2)
            -- set _22 3 x -> (1, 3)
            -- over _11 (\y -> y * 5) x -> (5, 2)
            -- over _22 (\y -> y * 5) x -> (1, 10)
            -- newL = choosing _1 _2
            -- view newL (Right x) -> 2
            -- view newL (Left x) -> 1
            -- set newL 10 (Left x) -> Left (10, 2)
            -- set newL 10 (Right x) -> Right (1, 10)
            -- over newL (\x -> x ** 7) (Right x) -> Right (1,128.0)
            -- over newL (\x -> x ** 7) (Left x) -> Left (1.0,2)
            -- (<%~) _1 (\x -> x + 3) x -> (4,(4,2))
            -- (<%~) _2 (\x -> x + 3) x -> (5,(1,5))
            -- (<<%~) _2 (\x -> x + 3) x -> (2,(1,5))
            -- (<<%~) _1 (\x -> x + 3) x -> (1,(4,2))

            

            -- ans <- runExceptT (runReaderT (evaluate evaluateRight) (Environment $ Map.fromList [("x", 1)]))
            -- case ans of 
            --     Right ansans -> ansans `shouldBe` 7  
       