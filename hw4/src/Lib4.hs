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
{-# LANGUAGE FlexibleInstances #-}


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
          , walker
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
import           System.FilePath            (takeFileName, (</>), replaceExtension, hasExtension, dropFileName)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Comonad            (Comonad, extract, duplicate, extend)
import           Control.Comonad.Traced     (Traced, runTraced)
import           Data.Traversable           (traverse)
import           Control.Monad.State        (MonadIO, MonadState, evalStateT, execStateT,
                                            get, modify, runStateT)
import           Text.Read                  (readMaybe)
import           Data.Monoid                (Endo, appEndo, Sum, getSum)

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
    deriving (Show, Eq)

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


data StateEnvironment = StateEnvironment { _fs :: [FS], _currPath :: FilePath, _fd :: [(Int, Int)] } deriving (Show, Eq)

-- putStrLn для Text -> IO ()

makeLenses ''StateEnvironment

printInfo :: (MonadState StateEnvironment m, MonadIO m) => Int -> Int -> m ()
printInfo filesInDir dirsInDir = do
    stateEnv <- get
    let path = _currPath $ stateEnv

    liftIO $ putStrLn $ "You in " ++ path
    liftIO $ putStrLn $ "Files from root " ++ path ++ ": " ++  show filesInDir
    liftIO $ putStrLn $ "Directories from " ++ path ++ ": " ++  show dirsInDir
    liftIO $ putStr "> "

getCommand :: String -> [String]
getCommand command = let com = words command in
    if (==) 1 $ length com 
        then com
        else let commandName = head com
                 commandArgs = readMaybe $ unwords $ drop 1 com
             in case commandArgs of 
                Just arg -> [commandName, arg]
                Nothing -> [commandName]


walkerImpl :: (MonadState StateEnvironment m, MonadIO m) => m ()
walkerImpl = do
    stateEnv <- get
    let fsOnPath = _fs stateEnv
    let fdOnPath = _fd stateEnv

    let currFS = head fsOnPath
    let (files, dirs) = head fdOnPath
    let filesOnPath = (+) files $ length $ (currFS ^.. contents . traversed . filtered (\x -> has _File x))
    let dirsOnPath = (+) dirs $ length $ (currFS ^.. contents . traversed . filtered (\x -> has _Dir x))

    printInfo filesOnPath dirsOnPath
    command <- liftIO getLine

    let com = getCommand command

    case com of
        ["cd", dirName] -> do
            let maybeDirToGo = currFS ^? cd dirName
            case maybeDirToGo of
                Just dirToGo -> do
                    modify (\se -> (%~) fs (dirToGo :) se)
                    modify (\se -> (%~) currPath (</> dirName) se)
                    modify (\se -> (%~) fd ((filesOnPath,dirsOnPath) :) se)
                Nothing -> liftIO $ putStr "Unknown directory, please try again\n"
        ["up"] -> if (==) 1 $ length fsOnPath
                  then liftIO $ putStr "You are trying to leave initial directory which is not permitted\n"
                  else do
                    modify (\se -> (%~) fs (drop 1) se)
                    modify (\se -> (%~) currPath dropFileName se)
                    modify (\se -> (%~) fd (drop 1) se)
        ["ls"] -> do
            let lsInDir = currFS ^.. ls
            liftIO $ putStr $ intercalate "\n" lsInDir ++ "\n"
        _ -> liftIO $ putStr "Unknown command, please try again\n"
    walkerImpl


walker :: FilePath -> IO ()
walker fp = do
    dir <- getDir fp
    runStateT (walkerImpl) (StateEnvironment [dir] fp [(0, 0)])
    return ()

-----------------------------------------------

class Monoid e => MonoidAction s e where
--  e - monoid, s - element from X
    act :: s -> e -> s

-- Действие моноида M на X : M x X -> X с операцией act(m, x) == m . x: 
-- 1. forall s, t in M, x in X: s . (t . x) = (s * t) . x где * - операция полугруппы
-- 2. e - identity element in M, forall x in X: e . x = x

instance MonoidAction [a] [a] where
    act :: [a] -> [a] -> [a]
    act slist elist = slist `mappend` elist

instance Monoid a => MonoidAction (Maybe a) (Maybe a) where 
    act :: (Maybe a) -> (Maybe a) -> (Maybe a)
    act s e'@(Just _) = s `mappend` e'
    act s Nothing = s

-- Endo { appEndo :: a -> a }, морфизм объекта категории в себя
    
instance MonoidAction s (Endo s) where 
    act :: s -> (Endo s) -> s
    act s endo = (appEndo endo) s

-- А потом можно и что-нибудь чуть-чуть сложней, типа

-- Sum { getSum :: a }

data Foo = Foo { fooList :: [Int], fooAcc :: Sum Double, fooDescription :: Text }

instance MonoidAction Foo [Int] where 
    act :: Foo -> [Int] -> Foo
    act foo' list = foo' { fooList = list `mappend` fooList foo' }

instance MonoidAction Foo (Sum Double) where
    act :: Foo -> (Sum Double) -> Foo
    act foo' sum' = foo' { fooAcc = sum' `mappend` fooAcc foo' }


data Renew s e a = Renew (e -> a) s

instance Functor (Renew s e) where
--  fmap :: (a -> b) -> (e -> a) -> s -> (e -> b) -> s
    fmap :: (a -> b) -> (Renew s e a) -> (Renew s e b)
    fmap f (Renew upd s) = Renew (f . upd) s

    -- act :: s -> e -> s
instance MonoidAction s e => Comonad (Renew s e) where 
--  extract :: (e -> a) -> s -> a
    extract :: (Renew s e a) -> a -- w a -> a
    extract (Renew upd _) = upd mempty
--  duplicate :: ((e -> a) -> s) -> ((e -> ((e -> a) -> s)) -> s)
    duplicate :: (Renew s e a) -> Renew s e (Renew s e a) -- w a -> w (w a)
    -- duplicate (Renew upd s) = Renew (\e -> Renew upd (act s e)) s -- WRONG
    duplicate (Renew upd s) = Renew (\e -> Renew (\e' -> upd $ e' `mappend` e) s) s -- RIGHT
--  extend :: (w a -> b) -> w a -> w b
    extend :: ((Renew s e a) -> b) -> (Renew s e a) -> (Renew s e b)  -- (w a -> b) -> w a -> w b
    extend func (Renew upd s) = Renew (\k -> func (Renew (\e' -> upd $ e' `mappend` k) s)) s -- fmap func (duplicate renew)
    

--WRONG
-- extract . duplicate   renew   = id renew == extract (duplicate renew)
-- extract (Renew (\e -> Renew upd (act s e)) s) = Renew upd (act s e) = Renew upd s

-- RIGHT
-- extract . duplicate   renew   = id renew == extract (duplicate renew)
-- extract (Renew (\e -> Renew (\e' -> upd $ e' `mappend` e) s) 
-- = (\e -> Renew (\e' -> upd $ e' `mappend` e) s) mempty
-- = Renew (\e' -> upd $ e' `mappend` mempty) s = Renew upd s


--WRONG
-- fmap extract . duplicate (Renew upd s) = id (Renew upd s) = fmap extract $ duplicate (Renew upd s)
-- fmap extract (Renew (\e -> Renew upd (act s e)) s) = Renew (extract . (\e -> Renew upd (act s e))) s
-- = Renew (\e' -> extract . (\e -> Renew upd (act s e)) e') s 
-- = Renew (\e' -> extract (Renew upd (act s e'))) s
-- = Renew (\e' -> extract (Renew upd _)) s

-- RIGHT
-- fmap extract . duplicate (Renew upd s) = id (Renew upd s) = fmap extract $ duplicate (Renew upd s)
-- fmap extract (Renew (\e -> Renew (\e' -> upd $ e' `mappend` e) s) s) 
-- = Renew (extract . (\e -> Renew (\e' -> upd $ e' `mappend` e) s)) s
-- = Renew (\e'' -> extract . (\e -> Renew (\e' -> upd $ e' `mappend` e) s) e'') s 
-- = Renew (\e'' -> extract ((\e -> Renew (\e' -> upd $ e' `mappend` e) s) e'')) s
-- = Renew (\e'' -> extract (Renew (\e' -> upd $ e' `mappend` e'') s)) s
-- = Renew (\e'' -> (\mempty -> upd $ mempty `mappend` e'')) s
-- = Renew (\e'' -> upd e'') s = Renew upd s


-- WRONG
-- duplicate . duplicate renew = fmap duplicate . duplicate
-- duplicate (Renew (\e -> Renew upd (act s e)) s) = Renew (\e' -> Renew (\e -> Renew upd (act s e)) (act s e')) s

-- fmap duplicate (Renew (\e -> Renew upd (act s e)) s) 
-- = Renew (duplicate . (\e -> Renew upd (act s e))) s
-- = Renew (\e' -> duplicate ((\e -> Renew upd (act s e)) e')) s
-- = Renew (\e' -> duplicate (Renew upd (act s e'))) s
-- = Renew (\e' -> Renew (\e -> Renew upd (act (act s e') e)) (act s e')) s
-- = Renew (\e' -> Renew (\e -> Renew upd (act s (e' <> e))) (act s e')) s

-- RIGHT
-- duplicate . duplicate renew = fmap duplicate . duplicate
-- duplicate (Renew (\e -> Renew (\e' -> upd $ e' `mappend` e) s) s) =
-- Renew (\e -> Renew (\e' -> (\k -> Renew (\k' -> upd $ k' `mappend` k) s) $ e' `mappend` e) s) s =
-- Renew (\e -> Renew (\e' -> (Renew (\k' -> upd $ k' `mappend` e' `mappend` e) s) s) s 

-- duplicate . duplicate renew = fmap duplicate . duplicate
-- fmap duplicate (Renew (\e -> Renew (\e' -> upd $ e' `mappend` e) s) s) 
-- = Renew (duplicate . (\e -> Renew (\e' -> upd $ e' `mappend` e) s)) s
-- = Renew (\k -> duplicate . (\e -> Renew (\e' -> upd $ e' `mappend` e) s) k) s
-- = Renew (\k -> duplicate ((\e -> Renew (\e' -> upd $ e' `mappend` e) s) k)) s
-- = Renew (\k -> duplicate (Renew (\e' -> upd $ e' `mappend` k) s)) s
-- = Renew (\k -> Renew (\e -> Renew (\e' -> (\k' -> upd $ k' `mappend` k) $ e' `mappend` e) s) s) s
-- = Renew (\k -> Renew (\e -> Renew (\e' -> upd $ e' `mappend` e `mappend` k) s) s) s
-- = Renew (\e -> Renew (\e' -> Renew (\k' -> upd $ k' `mappend` e' `mappend` e) s) s) s

---------------------------

-- newtype Traced m a = Traced { runTraced :: m -> a }

data ProjectSettings = ProjectSettings
     { settingsBenchs :: Bool  -- ^ enable benchmarks for project?
     , settingsGithub :: Bool  -- ^ set up github     for project?
     , settingsTravis :: Bool  -- ^ set up Travis CI  for project?
     }

data Project = Project
     { projectName :: Text
     , hasBenchs   :: Bool
     , hasGithub   :: Bool
     , hasTravis   :: Bool
     } deriving (Show)

type ProjectBuilder = ProjectSettings -> Project

buildProject :: Text -> ProjectBuilder
buildProject text = undefined