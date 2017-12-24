{-# LANGUAGE DeriveLift            #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE Unsafe                #-}



module Lib4
        (
            choseByIndices
          , choseByIndices'
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
          , buildProject
          , github
          , benchs
          , travis
        ) where

import           Data.List                  (find, intercalate)
import           Language.Haskell.TH        (Exp, Q, conT, lamE, mkName, reify,
                                             tupE, tupP, varE, varP, newName, conP, wildP)
import           Language.Haskell.TH.Syntax (Dec (..), Info (..), Name)
-- import Control.Monad (replicateM)
import           Control.Comonad            (Comonad, duplicate, extend, extract, (=>>))
import           Control.Lens               (Traversal, Traversal', each, filtered,
                                             folded, folding, has, makeLenses, makePrisms,
                                             preview, review, toListOf, traverse,
                                             traversed, (%~), (&), (.~), (^.), (^..),
                                             (^?), _Just, _head)
import           Control.Monad.IO.Class     (liftIO)
import           Data.Functor.Const         (Const (..), getConst)
import           Data.Functor.Identity      (Identity (..), runIdentity)
import           Data.Text                  (Text, append, pack, unpack)
import qualified Data.Text.IO               as TextIO (getLine, putStr, putStrLn)
import           Debug.Trace                (trace)
import           System.Directory           (doesDirectoryExist, doesFileExist,
                                             listDirectory)
import           System.FilePath            (dropFileName, replaceExtension,
                                             takeFileName, (</>))
-- import qualified       Control.Comonad      (Tree)
import           Control.Comonad.Traced     (Traced, runTraced, traced)
import           Control.Monad.Reader       (MonadReader, ask, runReaderT, replicateM)
import           Control.Monad.State        (MonadIO, MonadState, evalStateT, execStateT,
                                             get, modify, runStateT)
import           Data.Monoid                (Endo, Sum, appEndo, getSum)

choseByIndices :: Int -> [Int] -> Q Exp
choseByIndices n indices = do
    let namesIndiced = map (\x -> "x" ++ show x) indices
    let namesIndicedAll = map (\x -> "x" ++ show x) (take n [0..])
    lamE [tupP $ map (varP . mkName) namesIndicedAll] (tupE $ map (varE . mkName) namesIndiced)

tuple :: Int -> Q Exp
tuple n = do
    ns <- replicateM n (newName "x")
    lamE [foldr (\x y -> conP '(:) [varP x,y]) wildP ns] (tupE $ map varE ns)

choseByIndices' :: [Int] -> Q Exp
choseByIndices' indices = do
    let x = mkName "x"
    let size = length indices
    lamE [varP x] [| $(tuple size) $ map ((($(varE x)) ^.. each) !!) indices|]
    
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
deriveShow t = [d| instance ShowText $(conT t) where
                    showText x = pack (show x)
                |]

showText' :: Name -> Q [Dec]
showText' name = do
    info <- reify name
    case info of
        (TyConI NewtypeD{}) -> deriveShow name
        (TyConI DataD{})    -> deriveShow name
        TyConI TySynD{}     -> deriveShow name
        _                   -> error "Type should be newtype or data"

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
                            Left o  -> Left <$> l1 f o
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
                return $ Dir (takeFileName path) contentFS
            else error "Path is neither file path nor directory path"

--file = z
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
changeName fs fp = (%~) name (++ fp) fs

--6
findFstDir :: FS -> Maybe FilePath
findFstDir fs = (%~) _Just (^. name) $ find (has _Dir) (fs ^. contents ^.. folded)

--7
getFilesFromDir :: FS -> Maybe [FilePath]
-- getFileFromDir dir = dir ^. contents ^.. folded . filtered (\ x -> has _File x) . name
getFilesFromDir dir = (%~) _Just (\(_, cont) -> cont ^.. folded . filtered (has _File) . name) (dir ^? _Dir)


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
changeFileNames fs filename = (contents . traversed . filtered (has _File) . name %~ (const filename)) fs

changeFileExtensions :: FS -> FilePath -> FS
changeFileExtensions fs extension =
    (contents . traversed . filtered (has _File) . name %~ (`replaceExtension` extension)) fs

-- (dir ^.. cd "beautifuldir") >>= getAllNames
getAllNames :: FS -> [FilePath]
getAllNames fs = let fileNames = fs ^.. contents . traversed . filtered (has _File) . name
                     dirs = fs ^.. contents . traversed . filtered (has _Dir)
                 in (fs ^. name) : (fileNames ++ (dirs >>= getAllNames))

deleteDir :: FilePath -> FS -> FS
deleteDir filename fs = fs & contents .~
    fs ^.. contents . traversed . filtered (\x -> not $ has _Dir x && x ^. name == filename && ((==) 1 $ length $ x ^. contents))


data StateEnvironment = StateEnvironment { _fs :: [FS], _currPath :: FilePath, _fd :: [(Int, Int)] } deriving (Show, Eq)

-- putStrLn для Text -> IO ()

makeLenses ''StateEnvironment

printInfo :: (MonadReader RootPath m, MonadState StateEnvironment m, MonadIO m) => Int -> Int -> m ()
printInfo filesInDir dirsInDir = do
    stateEnv <- get
    root <- ask
    let path = stateEnv ^. currPath

    liftIO $ putStrLn $ "You're in " ++ path
    liftIO $ putStrLn $ "Files from root " ++ root ++ ": " ++  show filesInDir
    liftIO $ putStrLn $ "Directories from " ++ root ++ ": " ++  show dirsInDir
    liftIO $ putStr "> "

getCommand :: String -> [String]
getCommand command = let com = words command in
    if (==) 1 $ length com
        then com
        else let commandName = head com
                 commandArgs = dropWhile (== ' ') $ drop (length commandName) command
             in [commandName, commandArgs]


type RootPath = FilePath

walkerImpl :: (MonadReader RootPath m, MonadState StateEnvironment m, MonadIO m) => m ()
walkerImpl = do
    stateEnv <- get

    let currFS = head $ stateEnv ^. fs
    let (files, dirs) = head $ stateEnv ^. fd
    let filesOnPath = (+) files $ length (currFS ^.. contents . traversed . filtered (has _File))
    let dirsOnPath = (+) dirs $ length (currFS ^.. contents . traversed . filtered (has _Dir))

    printInfo filesOnPath dirsOnPath
    command <- liftIO getLine

    let com = getCommand command

    case com of
        ["cd", dirName] -> do
            let maybeDirToGo = currFS ^? cd dirName
            case maybeDirToGo of
                Just dirToGo -> do
                    modify ((%~) fs (dirToGo :))
                    modify ((%~) currPath (</> dirName))
                    modify ((%~) fd ((filesOnPath,dirsOnPath) :))
                Nothing -> liftIO $ putStr "Unknown directory, please try again\n"
        ["up"] -> if (==) 1 $ length $ stateEnv ^. fs
                  then liftIO $ putStr "You are trying to leave an initial directory which is not permitted\n"
                  else do
                    modify ((%~) fs (drop 1))
                    modify ((%~) currPath dropFileName)
                    modify ((%~) fd (drop 1))
        ["ls"] -> do
            let lsInDir = currFS ^.. ls
            liftIO $ putStr $ intercalate "\n" lsInDir ++ "\n"
        _ -> liftIO $ putStr "Unknown command, please try again\n"
    walkerImpl


walker :: FilePath -> IO ()
walker fp = do
    dir <- getDir fp
    runReaderT (runStateT walkerImpl (StateEnvironment [dir] fp [(0, 0)])) fp
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
    act :: Maybe a -> Maybe a -> Maybe a
    act s e'@(Just _) = s `mappend` e'
    act s Nothing     = s

-- Endo { appEndo :: a -> a }, морфизм объекта категории в себя

instance MonoidAction s (Endo s) where
    act :: s -> Endo s -> s
    act s endo = (appEndo endo) s

-- А потом можно и что-нибудь чуть-чуть сложней, типа

-- Sum { getSum :: a }

data Foo = Foo { fooList :: [Int], fooAcc :: Sum Double, fooDescription :: Text }

instance MonoidAction Foo [Int] where
    act :: Foo -> [Int] -> Foo
    act foo' list = foo' { fooList = list `mappend` fooList foo' }

instance MonoidAction Foo (Sum Double) where
    act :: Foo -> Sum Double -> Foo
    act foo' sum' = foo' { fooAcc = sum' `mappend` fooAcc foo' }


data Renew s e a = Renew (e -> a) s

instance Functor (Renew s e) where
--  fmap :: (a -> b) -> (e -> a) -> s -> (e -> b) -> s
    fmap :: (a -> b) -> Renew s e a -> Renew s e b
    fmap f (Renew upd s) = Renew (f . upd) s

    -- act :: s -> e -> s
instance MonoidAction s e => Comonad (Renew s e) where
--  extract :: (e -> a) -> s -> a
    extract :: Renew s e a -> a -- w a -> a
    extract (Renew upd _) = upd mempty
--  duplicate :: ((e -> a) -> s) -> ((e -> ((e -> a) -> s)) -> s)
    duplicate :: Renew s e a -> Renew s e (Renew s e a) -- w a -> w (w a)
    -- duplicate (Renew upd s) = Renew (\e -> Renew upd (act s e)) s -- WRONG
    duplicate (Renew upd s) = Renew (\e -> Renew (\e' -> upd $ e' `mappend` e) s) s -- RIGHT
--  extend :: (w a -> b) -> w a -> w b
    extend :: (Renew s e a -> b) -> Renew s e a -> Renew s e b  -- (w a -> b) -> w a -> w b
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
     } deriving (Show)

data Project = Project
     { projectName :: Text
     , hasBenchs   :: Bool
     , hasGithub   :: Bool
     , hasTravis   :: Bool
     } deriving (Show)

-- type ProjectBuilder = ProjectSettings -> Project -- Traced ProjectSettings Project

type ProjectBuilder' = Traced ProjectSettings Project
-- extract :: Traced m a -> a
-- extend :: (Traced m a -> b) -> Traced m a -> Traced m b

defaultSettings :: ProjectSettings
defaultSettings = ProjectSettings False False False

instance Monoid ProjectSettings where
    mempty = defaultSettings
    s1 `mappend` s2 = ProjectSettings (settingsBenchs s1 || settingsBenchs s2)
                                      (settingsGithub s1 || settingsGithub s2)
                                      (settingsTravis s1 || settingsTravis s2)

--        (ProjectSettings -> Project) -> Project
github :: ProjectBuilder' -> Project
github builder = runTraced builder (defaultSettings { settingsGithub = True })

benchs :: ProjectBuilder' -> Project
benchs builder = runTraced builder (defaultSettings { settingsBenchs = True })

travis :: ProjectBuilder' -> Project
travis builder = if hasGithub $ (runTraced builder) defaultSettings
                 then runTraced builder (defaultSettings { settingsTravis = True })
                 else runTraced builder defaultSettings

mkDefProj :: Text -> ProjectSettings -> Project
mkDefProj name' settings = Project name' (settingsBenchs settings) (settingsGithub settings) (settingsTravis settings)

buildProject :: Text -> ProjectBuilder'
buildProject text = traced (mkDefProj text)

data Tree a = Node a [Tree a]

instance Functor Tree where
    fmap :: (a -> b) -> Tree a -> Tree b
    fmap f (Node a childrens) = Node (f a) (childrens >>= (\child -> [fmap f child]))

instance Comonad Tree where
    extract :: Tree a -> a -- w a -> a
    extract (Node a _) = a
    duplicate :: Tree a -> Tree (Tree a) -- w a -> w (w a)
    duplicate (Node a childrens) = Node (Node a childrens) (childrens >>= (\child -> [duplicate child]))
    extend :: (Tree a -> b) -> Tree a -> Tree b -- (w a -> b) -> w a -> w b
    extend func node = fmap func (duplicate node)

-- extract . duplicate   tree   = id renew == extract (duplicate (Node a childrens))
-- extract (Node (Node a childrens) (childrens >>= (\child -> [duplicate child])))
-- =  (Node a childrens)

-- fmap extract . duplicate (Node a childrens) = id (Node a childrens) = fmap extract $ duplicate (Node a childrens)
-- fmap extract $ (Node (Node a childrens) (childrens >>= (\child -> [duplicate child])))
-- = {Node (extract a) (childrens >>= (\child -> [fmap extract child]))}
-- = (Node (extract (Node a childrens)) ((childrens >>= (\child -> [duplicate child])) >>= (\child' -> [fmap extract child']))
-- = Node a ((childrens >>= (\child -> [duplicate child])) >>= (\child' -> [fmap extract child']))
-- = Node a (childrens >>= (\x -> (\child -> [duplicate child]) x >>= (\child' -> [fmap extract child'])))
-- = Node a ((childrens >>= (\child -> [duplicate child])) >>= (\child' -> [fmap extract child']))
-- = Node a (childrens >>= (\x -> [duplicate x] >>= (\child' -> [fmap extract child']))) {[k] == return k, return a >>= f == f a}
-- = Node a (childrens >>= (\x -> (\child' -> [fmap extract child']) duplicate x))
-- = Node a (childrens >>= (\x -> [fmap extract (duplicate x)]))
-- = Node a (childrens >>= (\x -> [fmap extract (duplicate x)]))

-- duplicate . duplicate (Node a childrens) = fmap duplicate . duplicate (Node a childrens)

-- duplicate (Node (Node a childrens) (childrens >>= (\child -> [duplicate child])))
-- = Node ((Node (Node a childrens) (childrens >>= (\child -> [duplicate child]))))
--     ((childrens >>= (\child -> [duplicate child])) >>= (\child -> [duplicate child]))

---------------------------------------------------

-- List a = Nil | Cons a (List a) ~ [a], List(a) = 1 / (1 - a) => [a] = 1 / (1 - a)
-- Tree a = Node a [Tree a]
-- Tree a = a * 1 / (1 - (Tree a))
-- (Tree a)' = (a * 1 / (1 - (Tree a)))' = a' * 1 / (1 - (Tree a)) + a * (1 / (1 - (Tree a)))'
-- (Tree a)' = 1 / (1 - (Tree a)) + a * Tree'(a) * (1 / (1 - (Tree a))) ^ 2
-- (Tree a)' - Tree'(a) * a * (1 / (1 - (Tree a))) ^ 2 = 1 / (1 - (Tree a))
-- (Tree a)' * (1 - a * (1 / (1 - (Tree a))) ^ 2) = 1 / (1 - (Tree a))
-- (Tree a)' = 1 / (1 - (Tree a)) * 1 / (1 - a * (1 / (1 - (Tree a))) ^ 2)
-- {1 / (1 - (Tree a)) == [Tree a]}
-- {1 / (1 - a * (1 / (1 - (Tree a))) ^ 2) == [a * [Tree a] * [Tree a]]}
-- (Tree a)' = [Tree a] * [a * [Tree a] * [Tree a]]

data ListEntry  a = LE a [Tree a] [Tree a]
data RoseTreeZipper a = RTZ a [Tree a] [ListEntry a]