{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE TupleSections                #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}


module Lib4 
        (
          choseByIndices
          , MyData (..)
          , MyType
          , A (..)
          , MyNewType (..)
          , ShowText
          , showText
          , D (..)
          , showText'
          , set
          , view
          , over
          , _1
          , _2
          , MyLens
          , MyLens'
          , lens
          , choosing
          , (<%~)
          , (<<%~)
          , getDir
          , showDirContent
          , showDirName
          , showFileName
          , setName
          , changeName
          , findFstDir
          , getFilesFromDir
          , cd
          , ls
          , file
          , changeFileNames
          , changeFileExtensions
          , getAllNames
          , deleteDir
        ) where

import Language.Haskell.TH (Exp, Q, lamE, tupE, varE, mkName, varP, tupP, reify, runIO, conT)
import Language.Haskell.TH.Syntax (Name, Dec (..), Info (..))
import Data.List (intercalate, find, filter)
-- import Control.Monad (replicateM)
import           Data.Functor.Identity      (Identity (..), runIdentity)
import           Data.Functor.Const         (Const (..), getConst)
import           Data.Text                  (Text, pack)
import           Debug.Trace                (trace)
import           System.Directory           (listDirectory, doesFileExist, doesDirectoryExist)
import           Control.Lens               (makeLenses, makePrisms, (^.), (.~), (%~), (&), (^?), Traversal', 
                                            _Just, preview, review, traversed, has, filtered, folded, (^..), 
                                            toListOf, folding, each, traverse, Traversal, _head)
import           System.FilePath            (takeFileName, (</>), replaceExtension, hasExtension)
import           Control.Monad.IO.Class     (liftIO)
import Control.Comonad (Comonad)
import Data.Traversable (traverse)


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

----------------------------

type MyLens s t a b = forall f . Functor f => (a -> f b) -> s -> f t

type MyLens' s a  = MyLens s s a a

-- MyLens' obj field -> field -> obj -> obj
set  :: MyLens' s a -> a -> s -> s         -- set    value (setter)
set len field obj = runIdentity $ len (\_ -> Identity field) obj

-- MyLens' obj field -> obj -> field
view :: MyLens' s a -> s -> a              -- lookup value (getter)
view len obj = getConst $ len Const obj

-- MyLens' obj field -> (field -> field) -> obj -> obj
over :: MyLens' s a -> (a -> a) -> s -> s  -- change value (modifier)
over len changeFunc obj = runIdentity $ len (Identity . changeFunc) obj

-- _1 :: Functor f => (a -> f b) -> (a, x) -> f (b, x)
_1 :: MyLens (a, x) (b, x) a b
_1 f (a, x) = fmap (, x) (f a)

-- _2 :: Functor f => (a -> f b) -> (x, a) -> f (x, b)
_2 :: MyLens (x, a) (x, b) a b
_2 f (x, a) = fmap (x, ) (f a)

-- forall f . Functor f => (s -> a) -> (s -> b -> t) -> (a -> f b) -> s -> f t
lens :: (s -> a) -> (s -> b -> t) -> MyLens s t a b
lens get_ set_ f obj = set_ obj <$> f (get_ obj)

-- Объединить две линзы в одну, которая работает с Either.
choosing :: MyLens s1 t1 a b -- (a -> f b) -> s1 -> f t1
         -> MyLens s2 t2 a b -- (a -> f b) -> s2 -> f t2
         -> MyLens (Either s1 s2) (Either t1 t2) a b -- (a -> f b) -> Either s1 s2 -> f (Either t1 t2)
choosing l1 l2 f obj = case obj of
                            --  o :: s1
                            Left o -> Left <$> l1 f o
                            --  o :: s2
                            Right o -> Right <$> l2 f o

-- Изменить цель линзы и вернуть новый результат. Постарайтесь
-- реализовать без анонимных функций и определений своих функций
-- ((a -> f b) -> s -> f t) -> (a -> b) -> s -> (b, t)
(<%~) :: MyLens s t a b -> (a -> b) -> s -> (b, t)
(<%~) l f s = (getConst $ l (Const . f) s, runIdentity $ l (Identity . f) s)

-- Изменить цель линзы, но вернуть старый результат.
-- ((a -> f b) -> s -> f t) -> (a -> b) -> s -> (a, t)
(<<%~) :: MyLens s t a b -> (a -> b) -> s -> (a, t)
(<<%~) l f s = (getConst $ l Const s, runIdentity $ l (Identity . f) s)

------------------------------

data FS 
    = Dir 
          { _name     :: FilePath  -- название папки, не полный путь
          , _contents :: [FS]
          }
    | File
          { _name     :: FilePath  -- название файла, не полный путь
          }
    deriving (Show)

makeLenses ''FS
makePrisms ''FS

getDir :: FilePath -> IO FS
getDir path = do
    x <- doesFileExist path
    y <- doesDirectoryExist path
    if x 
        then return $ File $ takeFileName path
        else if y 
            then do
                content <- listDirectory path
                contentFS <- mapM (getDir . (</>) path) content
                return $ Dir (takeFileName path) $ contentFS
            else error "Path is neither file path nor directory path"

--file = File {_name = ".DS_Store"}
--dir <- getDir "/Users/alice/nicetestdir"

--1            
showDirContent :: FS -> [FS]
showDirContent dir = dir ^. contents

--2
showDirName :: FS -> Maybe FilePath
--       Maybe (FilePath, [FS])   Maybe (b1, b) -> Maybe b1
showDirName dir = (dir ^? _Dir) & (_Just %~ fst)

--3
showFileName :: FS -> FilePath
showFileName fs = fs ^. _File

--4
setName :: FS -> FS
setName fs = fs & name .~ "/"

--5
changeName :: FS -> FilePath -> FS
changeName fs fp = (%~) name (\n -> n ++ fp) fs

--6
findFstDir :: FS -> Maybe FilePath
findFstDir fs = (%~) _Just (\x -> x ^. name) $ find (\x -> has _Dir x) (fs ^. contents ^.. folded)

--7
getFilesFromDir :: FS -> Maybe [FilePath]
-- getFileFromDir dir = dir ^. contents ^.. folded . filtered (\ x -> has _File x) . name
getFilesFromDir dir = (%~) _Just (\(_, cont) -> cont ^.. folded . filtered (\x -> has _File x) . name) (dir ^? _Dir)


-- type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t

cd :: FilePath -> Traversal' FS FS
cd path = contents . traversed . filtered (\x -> has _Dir x && x ^. name == path)

-- ls' :: Traversal' FS [FilePath]
-- ls' = contents . each . (lens (\ s -> toListOf name s) (\ s (x : []) -> s & name .~ x))

ls :: Traversal' FS FilePath
ls = contents . traverse . name

file :: FilePath -> Traversal' FS FilePath
file filename = contents . traversed . filtered (\x -> has _File x && x ^. name == filename) . name

changeFileNames :: FS -> FilePath -> FS
changeFileNames fs filename = (contents . traversed . filtered (\x -> has _File x) . name %~ (\_ -> filename)) fs

changeFileExtensions :: FS -> FilePath -> FS
changeFileExtensions fs extension = 
    (contents . traversed . filtered (\x -> has _File x) . name %~ (\filename -> replaceExtension filename extension)) fs

-- (dir ^.. cd "beautifuldir") >>= getAllNames
getAllNames :: FS -> [FilePath]
getAllNames fs = let fileNames = fs ^.. contents . traversed . filtered (\x -> has _File x) . name
                     dirs = fs ^.. contents . traversed . filtered (\x -> has _Dir x) 
                 in (fs ^. name) : (fileNames ++ (dirs >>= getAllNames))
                    
deleteDir :: FilePath -> FS -> FS
deleteDir filename fs = fs & contents .~ 
    fs ^.. contents . traversed . filtered (\x -> not $ has _Dir x && x ^. name == filename && ((length $ x ^. contents) == 1))

-----------------------------------------------

data Renew s e a = Renew (e -> a) s

class Monoid e => MonoidAction s e where
    act :: s -> e -> s

instance Functor (Renew s e) where
    fmap :: (a -> b) -> f a -> f b
    fmap f fa = undefined

instance MonoidAction s e => Comonad (Renew s e)
