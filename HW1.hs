module Lib
(
    randomIntList,
    --Block1
    order3,
    highestBit,
    smartReplicate,
    contains,

    --Block2
    removeAt,
    collectEvery,
    stringSum,
    split,
    mergeSort,

    --Block3
    DaysOfWeek (..),
    numOfDay,
    dayFromNum,
    nextDay,
    afterDays,
    isWeekend,
    daysToParty,
    Creature (..),
    CreatureStats (..),
    fighting,
    to3D,
    vecLength,
    vecSum,
    vecMul,
    dist,
    vecMulVect,
    Nat (..),
    natSum,
    natMul,
    natSub,
    natToNum,
    isEven,
    natDiv,
    natMod,
    natGcd,
    Tree (..),
    isEmptyTree,
    sizeTree,
    findElemTree,
    insertIntoTree,
    fromList,
    --Block4
    splitOn,
    joinWith,
    maybeConcat,
    eitherConcat
) where


import           Data.Char       (isDigit, isSpace)
import           Data.Either     (lefts, rights)
import           Data.Foldable   (toList)
import           Data.List       (sort)
import           Data.Maybe      (fromJust, fromMaybe, isNothing)
import           Data.Monoid     as Monoid (Any (..), Sum (..), mempty)
import           Data.Semigroup  as Semigroup hiding (Endo, mempty)
import           Numeric.Natural
import           Prelude         hiding (mempty)
import           System.Random   (newStdGen, randomRs)

randomIntList :: Int -> Int -> Int -> IO [Int]
randomIntList n from to = take n . randomRs (from, to) <$> newStdGen

------------Block1

order3 :: Ord a => (a, a, a) -> (a, a, a)
order3 (x, y, z) =
    let [x', y', z'] = sort [x, y, z] in (x', y', z')

highestBit :: Natural -> Maybe (Natural, Natural)
highestBit x
        | x == 0     = Nothing
        | x == 1     = Just (1, 0)
        | otherwise =
            let s = highestBit (x `div` 2) in
                (if isNothing s then s else
                    (let j = fromJust s in Just (fst j * 2, snd j + 1)))

smartReplicate :: [Int] -> [Int]
smartReplicate = concatMap (\x -> replicate x x)

contains :: Eq a => a -> [[a]] -> [[a]]
contains e = filter $ elem e

-------------Block2

removeAt :: Int -> [a] -> (Maybe a, [a])
removeAt _ []           = (Nothing, [])
removeAt num (x : xs)
        | num < 0       = (Nothing, x : xs)
        | num == 0      = (Just x, xs)
        | otherwise     =
            let (maybeA, list) = removeAt (num - 1) xs in (maybeA, x : list)

collectEvery :: Int -> [a] -> Maybe ([a], [a])
collectEvery n list
        | n < 1     = Nothing
        | otherwise = Just $ collectEvery' 1 list
        where
            collectEvery' _ []       = ([], [])
            collectEvery' i (x : xs) =
                case i `mod` n of
                    0 -> let (oldlist, removed) = collectEvery' (i + 1) xs in
                           (oldlist, x : removed)
                    _ -> let (oldlist, removed) = collectEvery' (i + 1) xs in
                           (x : oldlist, removed)

stringSum :: String -> Maybe Int
stringSum = stringSum' []
        where
            stringSum' [] []            = Just 0

            stringSum' strToNum []      =
                if strToNum == ['-'] || strToNum == ['+']
                    then Nothing
                    else Just (read strToNum :: Int)

            stringSum' ['+'] (x : xs)   =
                if isDigit x
                    then stringSum' [x] xs
                    else Nothing

            stringSum' ['-'] (x : xs)   =
                if isDigit x
                    then stringSum' ('-' : [x]) xs
                    else Nothing

            stringSum' strToNum (x : xs)
                | isSpace x             =
                    if null strToNum
                        then stringSum' [] xs
                        else (let currInt = (read strToNum :: Int) in
                            let strSum = stringSum' [] xs in
                                if isNothing strSum
                                    then Nothing
                                    else Just (currInt + fromJust (stringSum' [] xs)))
                | x == '-' || x == '+' =
                    if null strToNum
                        then stringSum' [x] xs
                        else Nothing
                | not $ isDigit x       = Nothing
                | otherwise             = stringSum' (strToNum ++ [x]) xs


split :: [a] -> ([a], [a])
split [] = ([], [])
split [x] = ([x], [])
split (x:y:xys) = (x:xs, y:ys) where (xs, ys) = split xys


mergeSort :: Ord a => [a] -> [a]
mergeSort [x] = [x]
mergeSort [] = []
mergeSort arr =
    let mid = (length arr + 1) `div` 2 --------todo: fix splitting
    in merge (mergeSort $ fst $ splitAt mid arr) (mergeSort $ snd $ splitAt mid arr)
        where
            merge xs [] = xs
            merge [] ys = ys
            merge (x:xs) (y:ys) =
                if x > y
                    then y : merge (x : xs) ys
                    else x : merge xs (y : ys)

----------------Block3

data DaysOfWeek = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Show, Eq)

numOfDay :: DaysOfWeek -> Int
numOfDay Monday    = 0
numOfDay Tuesday   = 1
numOfDay Wednesday = 2
numOfDay Thursday  = 3
numOfDay Friday    = 4
numOfDay Saturday  = 5
numOfDay Sunday    = 6

dayFromNum :: Int -> DaysOfWeek
dayFromNum num
    | num == 0 = Monday
    | num == 1 = Tuesday
    | num == 2 = Wednesday
    | num == 3 = Thursday
    | num == 4 = Friday
    | num == 5 = Saturday
    | num == 6 = Sunday
    | otherwise = dayFromNum (num `mod` 7)

nextDay :: DaysOfWeek -> DaysOfWeek
nextDay day = (\x -> dayFromNum (x + 1)) $ numOfDay day

afterDays :: DaysOfWeek -> Int -> DaysOfWeek
afterDays day num = (\x -> dayFromNum (x + num)) $ numOfDay day

isWeekend :: DaysOfWeek -> Bool
isWeekend day
    | day == Saturday   = True
    | day == Sunday     = True
    | otherwise         = False

daysToParty :: DaysOfWeek -> Int
daysToParty Monday    = 4
daysToParty Tuesday   = 3
daysToParty Wednesday = 2
daysToParty Thursday  = 1
daysToParty Friday    = 7
daysToParty Saturday  = 6
daysToParty Sunday    = 5


data Creature
    = Knight { hp :: Int, attack :: Int }
    | Monster { hp :: Int, attack :: Int } deriving (Show)

class CreatureStats a where
    getHp :: a -> Int
    getAttackPoints :: a -> Int
    decreaseHp :: a -> Int -> a

instance CreatureStats Creature where
    getHp (Knight hpBar _)  = hpBar
    getHp (Monster hpBar _) = hpBar
    getAttackPoints (Knight _ attackBar)  = attackBar
    getAttackPoints (Monster _ attackBar) = attackBar
    decreaseHp (Knight hpBar attackBar) healthPoints = Knight (hpBar - healthPoints) attackBar
    decreaseHp (Monster hpBar attackBar) healthPoints = Monster (hpBar - healthPoints) attackBar

getReplyString :: Int -> String
getReplyString roundNum = case roundNum `mod` 2 of
    1 -> " attacked his opponent\n"
    0 -> " striked his opponent back\n"

fighting :: Creature -> Creature -> String
fighting fCreature@(Knight _ _) sCreature
    = fight fCreature sCreature "The fight has begun\n" 1
fighting fCreature sCreature
    = fight sCreature fCreature "The fight has begun\n" 1

fight :: Creature -> Creature -> String -> Int -> String
fight fCreature sCreature logFight roundNum =
    if (getHp sCreature - getAttackPoints fCreature) <= 0
        then logFight ++ "Round " ++ show roundNum ++ ": The fighter " ++ show fCreature ++ " win!\n"
        else fight (decreaseHp sCreature $ getAttackPoints fCreature) fCreature
            (logFight ++ "Round " ++ show roundNum ++ ": The fighter " ++ show fCreature ++ getReplyString roundNum)
            (roundNum + 1)


data Vector a = Vector2D a a | Vector3D a a a deriving (Show)

to3D :: Num a => Vector a -> Vector a
to3D (Vector2D x y) = Vector3D x y 0
to3D vector3D       = vector3D

vecLength :: Floating a => Vector a -> a
vecLength vector = let (Vector3D x y z) = to3D vector in
    sqrt $ x ** 2 + y ** 2 + z ** 2

vecSum :: Floating a => Vector a -> Vector a -> Vector a
vecSum x y = Vector3D (x1 + x2) (y1 + y2) (z1 + z2)
    where
        (Vector3D x1 y1 z1) = to3D x
        (Vector3D x2 y2 z2) = to3D y

vecMul :: Floating a => Vector a -> Vector a -> a
vecMul x y = x1 * x2 + y1 * y2 + z1 * z2
    where
        (Vector3D x1 y1 z1) = to3D x
        (Vector3D x2 y2 z2) = to3D y

dist :: Floating a => Vector a -> Vector a -> a
dist x y = sqrt $ (x1 - x2) ** 2 + (y1 - y2) ** 2 + (z1 - z2) ** 2
    where
        (Vector3D x1 y1 z1) = to3D x
        (Vector3D x2 y2 z2) = to3D y

vecMulVect :: Floating a => Vector a -> Vector a -> Vector a
vecMulVect x y = Vector3D (y1 * z2 - z1 * y2) (z1 * x2 - x1 * z2) (x1 * y2 - y1 * x2)
    where
        (Vector3D x1 y1 z1) = to3D x
        (Vector3D x2 y2 z2) = to3D y


data Nat = Z | S Nat deriving (Show)

natSum :: Nat -> Nat -> Nat
natSum Z x     = x
natSum (S x) z = natSum x (S z)

natMul :: Nat -> Nat -> Nat
natMul Z _     = Z
natMul _ Z     = Z
natMul (S Z) x = x
natMul (S x) z = natSum z $ natMul x z

natSub :: Nat -> Nat -> Maybe Nat
natSub x Z         = Just x
natSub Z (S _)     = Nothing
natSub (S x) (S y) = natSub x y

natToNum :: Nat -> Int
natToNum Z     = 0
natToNum (S x) = 1 + natToNum x

instance Eq Nat where
    (==) Z Z         = True
    (==) Z _         = False
    (==) _ Z         = False
    (==) (S x) (S y) = x == y

instance Ord Nat where
    compare Z Z         = EQ
    compare Z _         = LT
    compare _ Z         = GT
    compare (S x) (S y) = compare x y

isEven :: Nat -> Bool
isEven x = isEven' x 0
    where
        isEven' :: Nat -> Int -> Bool
        isEven' Z k     = (k `mod` 2) == 0
        isEven' (S n) k = isEven' n (k + 1)

natDiv :: Nat -> Nat -> Maybe Nat
natDiv _ Z = Nothing
natDiv x y = natDiv' x y Z
    where
        natDiv' x' y' z' =
            if x' >= y'
                then natDiv' (fromJust (natSub x' y')) y' (S z')
                else Just z'

natMod :: Nat -> Nat -> Maybe Nat
natMod _ Z = Nothing
natMod x y =
    if x >= y
        then natMod (fromJust (natSub x y)) y
        else Just x

natGcd :: Nat -> Nat -> Nat
natGcd x Z = x
natGcd x y = natGcd y (fromJust (natMod x y))


data Tree a = Leaf | Node a (Tree a) (Tree a) deriving (Show)

isEmptyTree :: Tree a -> Bool
isEmptyTree Leaf = True
isEmptyTree _    = False

sizeTree :: Tree a -> Int
sizeTree Leaf                = 0
sizeTree (Node _ left right) = 1 + sizeTree left + sizeTree right

findElemTree :: Ord a => Tree a -> a -> Bool
findElemTree Leaf _ = False
findElemTree (Node x left right) element =
    x == element || findElemTree left element || findElemTree right element

insertIntoTree :: Ord a => Tree a -> a -> Tree a
insertIntoTree Leaf element = Node element Leaf Leaf
insertIntoTree (Node x left right) element
    | element > x   = Node x left $ insertIntoTree right element
    | element < x   = Node x (insertIntoTree left element) right
    | otherwise     = Node x left right

fromList :: Ord a => [a] -> Tree a
fromList []     = Leaf
fromList [x]    = insertIntoTree Leaf x
fromList (x:xs) = insertIntoTree (fromList xs) x


----------Block4

instance Foldable Tree where
    foldMap _ Leaf = mempty
    foldMap f (Node x Leaf Leaf) = f x
    foldMap f (Node x Leaf right) = f x `mappend` foldMap f right
    foldMap f (Node x left Leaf) = f x `mappend` foldMap f left
    foldMap f (Node x left right) = f x `mappend `foldMap f left `mappend` foldMap f right
    foldr _ acc Leaf = acc
    foldr f acc (Node x Leaf Leaf) = f x acc
    foldr f acc (Node x Leaf right) = f x $ foldr f acc right
    foldr f acc (Node x left Leaf) = (\acc' -> foldr f acc' left) $ f x acc
    foldr f acc (Node x left right) = (\acc' -> foldr f acc' left) $ f x $ foldr f acc right

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn element = foldr (\x acc -> if x == element
    then [] : acc
    else let (y:ys) = acc in (x : y) : ys) [[]]

joinWith :: a -> [[a]] -> [a]
joinWith _ []         = []
joinWith element list = init $ foldr (\x acc -> x ++ [element] ++ acc) [] list

---------Block5

maybeConcat :: [Maybe [a]] -> [a]
maybeConcat []     = []
maybeConcat [x]    = fromMaybe [] x
maybeConcat (x:xs) = fromMaybe [] x `mappend` maybeConcat xs

eitherConcat :: (Monoid m, Monoid n) => [Either m n] -> Maybe (m, n)
eitherConcat list = if null leftElems || null rightElems
    then Nothing
    else Just (concatLeft leftElems, concatRight rightElems)
    where
        leftElems = lefts list
        rightElems = rights list
        concatLeft [x]      = x
        concatLeft (x : xs) = x `mappend` concatLeft xs
        concatRight [x]      = x
        concatRight (x : xs) = x `mappend` concatRight xs

data NonEmpty a = a :| [a] deriving (Show)

instance Semigroup (NonEmpty a) where
    (<>) (x :| xs) (y :| ys) = x :| (xs ++ [y] ++ ys)

newtype Identity a = Identity { runIdentity :: a } deriving (Show)

instance Semigroup a => Semigroup (Identity a) where
    (<>) x y = Identity $ runIdentity x <> runIdentity y

instance Monoid a => Monoid (Identity a) where
    mempty = Identity mempty
    mappend x y = Identity $ runIdentity x `mappend` runIdentity y


newtype Name = Name String deriving Show

instance Semigroup Name where
    (<>) (Name x) (Name y) = Name (x ++ "." ++ y)

instance Monoid Name where
    mempty = Name []
    mappend (Name x) (Name y)
        | null x && null y = mempty
        | null x = Name y
        | null y = Name x
        | otherwise = Name (x ++ "." ++ y)


newtype Endo a = Endo { getEndo :: a -> a }

instance Semigroup (Endo a) where
    (<>) x y = Endo (getEndo x . getEndo y)

instance Monoid (Endo a) where
    mempty = Endo id
    mappend x y = Endo (getEndo x . getEndo y)

newtype Arrow a b = Arrow { getArrow :: a -> b }

instance Semigroup b => Semigroup (Arrow a b) where
    (<>) x y = Arrow (\l -> getArrow x l <> getArrow y l)

instance Monoid b => Monoid (Arrow a b) where
    mempty = Arrow mempty
    mappend x y = Arrow (\l -> getArrow x l `mappend` getArrow y l)

instance Ord a => Semigroup (Tree a) where
    (<>) x y = fromList $ toList x ++ toList y

instance Ord a => Monoid (Tree a) where
    mempty = Leaf
    mappend x y = fromList $ toList x ++ toList y