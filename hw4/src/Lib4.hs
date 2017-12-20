{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveLift #-}


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

newtype MyNewType = MyNewType { zzz :: String }

data MyData = MyData { foo :: String, bar :: Int }

type MyType = String

data A = B | C 

data D = K { getInt :: Int } | L { getInt1 :: Int } 

----------------------

class ShowText a where 
    showText :: a -> Text

data HelpInfo = HelpInfo { className :: Name, fields :: [Name]}

-- test :: Q Exp -> Q Exp
-- test exp = do
--     test' <- runQ exp
--     name' <- case test' of 
--         (VarE name) -> return name
--         _ -> error ("WTF " ++ show test')
--     varE name'

-- showAlgebraicType :: Name -> HelpInfo -> Q [Dec]
-- showAlgebraicType name _ = do
--     [d|instance ShowText $(conT name) where
--         showText x = pack $ show $(test  $ [|x|]) |]
            

    
-- getHelpInfo :: Info -> HelpInfo
-- getHelpInfo (TyConI (DataD _ className' _ _ recC@((RecC recCName fields') : xs) _)) = HelpInfo className' (map (\(name,_,_) -> name) fields')
-- getHelpInfo (TyConI (DataD _ className' _ _ fields' _)) = HelpInfo className' (map (\(NormalC name _) -> name) fields')
-- getHelpInfo (TyConI (NewtypeD _ className' _ _ (RecC recCName fields') _)) = HelpInfo className' (map (\(name,_,_) -> name) fields')
-- getHelpInfo info = error ("Smth went really wrong " ++ show info)

-- showDataType :: Name -> HelpInfo -> Q [Dec]
-- showDataType name helpInfo = do
--     let showField :: Name -> Q Exp
--         showField name' = [|\x -> s ++ " = " ++ show ($(varE name') x)|] 
--           where s = nameBase name'
--     let showN :: Q Exp
--         showN = [| s |] 
--           where s = nameBase (className helpInfo)
--     let showFields :: Q Exp
--         showFields = listE $ map showField (fields helpInfo)
--     [d|instance ShowText $(conT name) where
--           showText x = pack ($(showN) ++ " { " ++ intercalate ", " (map ($ x) $showFields) ++ " }")|]

-- helpFunc :: Name -> Info -> Q [Dec]
-- helpFunc name dataT@(TyConI (NewtypeD _ _ _ _ _ _)) = showDataType name (getHelpInfo dataT)
-- helpFunc name dataT@(TyConI (DataD _ _ _ _ [RecC _ _] _)) = showDataType name (getHelpInfo dataT)
-- helpFunc name alg = showAlgebraicType name (getHelpInfo alg)

-- showTextInstance :: Name -> Q [Dec]
-- showTextInstance name = do
--     info <- reify name
--     helpFunc name info

-- TyConI (DataD [] Lib4.A [] Nothing [NormalC Lib4.B [],NormalC Lib4.C []] [])
-- TyConI (NewtypeD [] Lib4.MyNewType [] Nothing (RecC Lib4.MyNewType [(Lib4.zzz,Bang NoSourceUnpackedness NoSourceStrictness,ConT GHC.Base.String)]) [])
-- TyConI (DataD [] Lib4.MyData [] Nothing 
    -- [RecC Lib4.MyData [(Lib4.foo,Bang NoSourceUnpackedness NoSourceStrictness,ConT GHC.Base.String),
                      --  (Lib4.bar,Bang NoSourceUnpackedness NoSourceStrictness,ConT GHC.Types.Int)]] [])
-- TyConI (TySynD Lib4.MyType [] (ConT GHC.Base.String))
-- $(reify ''Single >>= runIO . print >> varE $ mkName "x" )
-- $(reify ''MyData >>= runIO . print >> (varE $ mkName "x") )

genPE :: Int -> Q ([PatQ], [ExpQ])
genPE n = do
    ids <- replicateM n (newName "x")
    return (map varP ids, map varE ids)

showClause :: Con -> Q Clause
showClause (NormalC name fieldsNames) = do
    let constructorName = nameBase name
    (pats,vars) <- genPE (length fieldsNames)
    let f []       = [| constructorName |]
        f (v:varss) = [| " " ++ show $v ++ $(f varss) |]
    clause [conP name pats]
            (normalB (f vars)) []
showClause (RecC name fieldsNames) = do
    let names = (map (\(name',_,_) -> name') fieldsNames)
    let constructorName = nameBase name
    let showField :: Name -> Q Exp
        showField name' = [|\x -> s ++ " = " ++ show ($(varE name') x)|] 
          where s = nameBase name'
    let showFields :: Q Exp
        showFields = listE $ map showField names
    let patName = mkName "x"
    let test = [|constructorName  ++ " { " ++ intercalate ", " (map ($ $(varE patName)) $showFields) ++ " } " |]
    clause [asP patName $ conP name (replicate (length names) wildP)]
        (normalB test) []

deriveDataDShow :: Name -> Q [Dec]
deriveDataDShow t = do
    TyConI (DataD _ _ _ _ constructors _) <- reify t

    showbody <- mapM showClause constructors

    d <- [d| instance Show $(conT t) where
                show _ = "text"
         |]
    let    [InstanceD Nothing [] (AppT showt cont) [FunD showf _text]] = d
    return [InstanceD Nothing [] (AppT showt cont) [FunD showf showbody]]

deriveNewtypeDShow :: Name -> Q [Dec]
deriveNewtypeDShow t = do
    TyConI (NewtypeD _ _ _ _ cons@(RecC recCName fields') _) <- reify t

    showbody <- showClause cons

    d <- [d| instance Show $(conT t) where
                show _ = "text"
         |]
    let    [InstanceD Nothing [] (AppT showt cont) [FunD showf _text]] = d
    return [InstanceD Nothing [] (AppT showt cont) [FunD showf [showbody]]]

showNewtype :: Name -> HelpInfo -> Q [Dec]
showNewtype name helpInfo = do
    let showField :: Name -> Q Exp
        showField name' = [|\x -> s ++ " = " ++ show ($(varE name') x)|] 
          where s = nameBase name'
    let showN :: Q Exp
        showN = [| s |] 
          where s = nameBase (className helpInfo)
    let showFields :: Q Exp
        showFields = listE $ map showField (fields helpInfo)
    [d|instance Show $(conT name) where
          show x = ($(showN) ++ " { " ++ intercalate ", " (map ($ x) $showFields) ++ " }")|]

showText' :: Name -> Q [Dec]
showText' name = do
    info <- reify name
    case info of 
        (TyConI (NewtypeD _ _ _ _ _ _)) -> deriveNewtypeDShow name       
        (TyConI (DataD _ _ _ _ _ _)) -> deriveDataDShow name
        _ -> error "Type should be newtype or data"