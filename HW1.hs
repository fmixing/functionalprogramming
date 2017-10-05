import           Data.List       (sort)
import           Data.Char       (isSpace, isDigit)
import           Numeric.Natural
import           System.Random (newStdGen, randomRs)

randomIntList :: Int -> Int -> Int -> IO [Int]
randomIntList n from to = take n . randomRs (from, to) <$> newStdGen

------------Block1

order3 :: Ord a => (a, a, a) -> (a, a, a)
order3 (x, y, z) =
    let s = [x, y, z] in ((\l -> (l !! 0, l !! 1, l !! 2)) $ sort s)

highestBit1 :: Natural -> (Natural, Natural)
highestBit1 x
        | x == 0     = error "Such an element doesn't exist"
        | x == 1     = (1, 0)
        | otherwise =
            let s = highestBit1 (x `div` 2)
            in (fst s * 2, snd s + 1)

smartReplicate :: [Int] -> [Int]
smartReplicate = concatMap (\x -> replicate x x)

contains :: Eq a => a -> [[a]] -> [[a]]
contains e = filter (\x -> elem e x)

-------------Block2

removeAt :: Int -> [a] -> (Maybe a, [a])
removeAt _ []           = (Nothing, [])
removeAt num (x : xs) 
        | num < 0       = (Nothing, x : xs)
        | num == 0      = (Just x, xs)
        | otherwise     = 
            let (maybeA, list) = (removeAt (num - 1) xs) in (maybeA, x : list)


collectEvery :: Int -> [a] -> ([a], [a])
collectEvery n list 
        | n < 1     = error "The k should be positive"
        | otherwise = collectEvery' 1 list
        where 
            collectEvery' _ []       = ([], [])
            collectEvery' i (x : xs) =
                case (i `mod` n) of
                    0 -> let (oldlist, removed) = (collectEvery' (i + 1) xs)
                            in (oldlist, x : removed)
                    _ -> let (oldlist, removed) = (collectEvery' (i + 1) xs)
                            in (x : oldlist, removed)

stringSum :: String -> Int
stringSum str = stringSum' [] str
        where
            stringSum' [] []            = 0

            stringSum' strToNum []      = 
                case (strToNum == ['-'] || strToNum == ['+']) of 
                    False -> read strToNum :: Int
                    True -> error "The string should look like Nums separated by WSs"

            stringSum' ['+'] (x : xs)   =
                case (isDigit x) of 
                    True -> stringSum' [x] xs
                    False -> error "The string should look like Nums separated by WSs"
            
            stringSum' ['-'] (x : xs)   =
                case (isDigit x) of 
                    True -> stringSum' (['-'] ++ [x]) xs
                    False -> error "The string should look like Nums separated by WSs"

            stringSum' strToNum (x : xs)
                | isSpace x             = 
                    case (strToNum == []) of 
                        True -> stringSum' [] xs
                        False -> let currInt = (read strToNum :: Int) in (currInt + (stringSum' [] xs))
                | (x == '-' || x == '+')=
                    case (strToNum == []) of 
                        True -> stringSum' [x] xs
                        False -> error "The string should look like Nums separated by WSs"
                | not $ isDigit x       = error "The string should contain only WS, -/+ and Num symbols"
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
                case (x > y) of 
                    True -> y : merge (x:xs) ys
                    False -> x : merge xs (y:ys)


----------------Block3

data DaysOfWeek = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Show, Eq)

numOfDay :: DaysOfWeek -> Int
numOfDay Monday = 0
numOfDay Tuesday = 1
numOfDay Wednesday = 2
numOfDay Thursday = 3
numOfDay Friday = 4
numOfDay Saturday = 5
numOfDay Sunday = 6

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
daysToParty Monday = 4
daysToParty Tuesday = 3
daysToParty Wednesday = 2
daysToParty Thursday = 1
daysToParty Friday = 7
daysToParty Saturday = 6
daysToParty Sunday = 5


data Creature  
    = Knight { hp :: Int, attack :: Int } 
    | Monster { hp :: Int, attack :: Int } deriving (Show)

class CreatureStats a where
    getHp :: a -> Int
    getAttackPoints :: a -> Int
    decreaseHp :: a -> Int -> a

instance CreatureStats Creature where
    getHp (Knight hpBar _) = hpBar
    getHp (Monster hpBar _) = hpBar
    getAttackPoints (Knight _ attackBar) = attackBar
    getAttackPoints (Monster _ attackBar) = attackBar
    decreaseHp (Knight hpBar attackBar) healthPoints = Knight (hpBar - healthPoints) attackBar
    decreaseHp (Monster hpBar attackBar) healthPoints = Monster (hpBar - healthPoints) attackBar
   
getReplyString :: Int -> String
getReplyString roundNum = case (roundNum `mod` 2) of
    1 -> " attacked his opponent\n"
    0 -> " striked his opponent back\n"

fighting :: Creature -> Creature -> String
fighting fCreature@(Knight _ _) sCreature 
    = fight fCreature sCreature ("The fight has begun\n") 1
fighting fCreature sCreature 
    = fight sCreature fCreature ("The fight has begun\n") 1

fight :: Creature -> Creature -> String -> Int -> String
fight fCreature sCreature logFight roundNum = 
    case ((getHp sCreature) - getAttackPoints fCreature) <= 0 of
        True -> logFight ++ "Round " ++ show roundNum ++ ": The fighter " ++ show fCreature ++ " win!\n"
        False -> 
            fight (decreaseHp sCreature $ getAttackPoints fCreature) fCreature 
                (logFight ++ "Round " ++ show roundNum ++ ": The fighter " ++ show fCreature ++ getReplyString roundNum)
                (roundNum + 1)


data Vector a = Vector2D a a | Vector3D a a a deriving (Show)

to3D :: Num a => Vector a -> Vector a
to3D (Vector2D x y) = (Vector3D x y 0)
to3D vector3D = vector3D

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
natSum Z x = x
natSum (S x) z = natSum x (S z)

natMul :: Nat -> Nat -> Nat
natMul Z _ = Z
natMul _ Z = Z
natMul (S Z) x = x
natMul (S x) z = (\l -> natSum z l) $ natMul x z

natSub :: Nat -> Nat -> Nat
natSub x Z = x
natSub Z (S _) = error "The first Nat number should be greater"
natSub (S x) (S y) = natSub x y

natToNum :: Nat -> Int
natToNum Z = 0
natToNum (S x) = 1 + natToNum x

instance Eq Nat where
    (==) Z Z = True
    (==) Z _ = False
    (==) _ Z = False
    (==) (S x) (S y) = x == y


instance Ord Nat where
    compare Z Z = EQ
    compare Z _ = LT
    compare _ Z = GT
    compare (S x) (S y) = compare x y

isEven :: Nat -> Bool
isEven x = isEven' x 0
    where
        isEven' :: Nat -> Int -> Bool
        isEven' Z k = (k `mod` 2) == 0
        isEven' (S n) k = isEven' n (k + 1)

natDiv :: Nat -> Nat -> Nat
natDiv _ Z = error "You should not divide by zero"
natDiv x y = natDiv' x y Z
    where
        natDiv' x' y' z' = 
            case (x' >= y') of
                True -> natDiv' (natSub x' y') y' (S z')
                False -> z'

natMod :: Nat -> Nat -> Nat
natMod _ Z = error "You should not divide by zero"
natMod x y = 
    case (x >= y) of
        True -> natMod (natSub x y) y
        False -> x
            

natGcd :: Nat -> Nat -> Nat
natGcd x Z = x
natGcd x y = natGcd y (natMod x y)


data Tree a = Leaf | Node a (Tree a) (Tree a)
