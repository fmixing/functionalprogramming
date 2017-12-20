{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE TypeSynonymInstances #-}


module Lib4 
        (
          choseByIndices
          , MyData (..)
          , MyType
          , A (..)
          , MyNewType (..)
        --   , showTextInstance
          , ShowText
          , showText
        --   , test
          , D (..)
        --   , deriveShow
          , showText'
        ) where

import Language.Haskell.TH 
import Language.Haskell.TH.Syntax
import Data.List (intercalate, head)
import Control.Monad (replicateM)
-- (Exp, Q, lamE, tupE, varE, mkName, varP, tupP, reify, runIO)
import qualified Data.Map.Strict as Map (Map, empty, fromList, lookup)
import           Data.Text                  (Text, append, pack, unpack)
import           Debug.Trace                (trace)


choseByIndices :: Int -> [Int] -> Q Exp
choseByIndices n indices = do
    let namesIndiced = map (\x -> "x" ++ show x) indices
    let namesIndicedAll = map (\x -> "x" ++ show x) (take n [0..])
    lamE [tupP $ map (varP . mkName) namesIndicedAll] (tupE $ map (varE . mkName) namesIndiced)

-----------------

newtype MyNewType = MyNewType { zzz :: String } deriving (Show)

data MyData = MyData { foo :: String, bar :: Int } deriving (Show)

type MyType = String

data A = B | C  deriving (Show)

data D = K { getInt :: Int } | L { getInt1 :: Int } deriving (Show)

----------------------

--- TyConI (DataD [] Lib4.A [] Nothing [NormalC Lib4.B [],NormalC Lib4.C []] [])
--- TyConI (NewtypeD [] Lib4.MyNewType [] Nothing (RecC Lib4.MyNewType [(Lib4.zzz,Bang NoSourceUnpackedness NoSourceStrictness,ConT GHC.Base.String)]) [])
--- TyConI (DataD [] Lib4.MyData [] Nothing 
    -- [RecC Lib4.MyData [(Lib4.foo,Bang NoSourceUnpackedness NoSourceStrictness,ConT GHC.Base.String),
                      --  (Lib4.bar,Bang NoSourceUnpackedness NoSourceStrictness,ConT GHC.Types.Int)]] [])
--- TyConI (TySynD Lib4.MyType [] (ConT GHC.Base.String))
--- $(reify ''Single >>= runIO . print >> varE $ mkName "x" )
--- $(reify ''MyData >>= runIO . print >> (varE $ mkName "x") )

class ShowText a where 
    showText :: a -> Text

deriveShow :: Name -> Q [Dec]
deriveShow t = do
    [d| instance ShowText $(conT t) where
            showText x = pack (show x)
        |]

showText' :: Name -> Q [Dec]
showText' name = do
    info <- reify name
    case info of 
        (TyConI (NewtypeD _ _ _ _ _ _)) -> deriveShow name       
        (TyConI (DataD _ _ _ _ _ _)) -> deriveShow name
        TyConI (TySynD _ _ _) -> deriveShow name
        _ -> error "Type should be newtype or data"