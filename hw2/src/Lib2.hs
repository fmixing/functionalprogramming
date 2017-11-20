{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExplicitForAll            #-}
{-# LANGUAGE ExplicitNamespaces        #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE TypeOperators             #-}

module Lib2
       (
         eval
       , Expr (..)
       , partial
       , type (~>)
       , total
       , apply
       , applyOrElse
       , withDefault
       , isDefinedAt
       , orElse
       , bin
       , combinations
       , permutations
       , Parser (..)
       , satisfy
       , char
       , posInt
       , Employee (..)
       , parseName
       , parsePhone
       , abParser
       , abParser_
       , intPair
       , intOrUppercase
       , zeroOrMore
       , oneOrMore
       , spaces
       , ident
       , SExpr (..)
       , Ident
       , Atom (..)
       , parseSExpr
       ) where

import qualified Control.Applicative ((<|>))
import qualified Control.Category    (Category, id, (.))
import qualified Control.Monad       (void, (>=>), (>>=))
import qualified Data.Char           (isAlpha, isAlphaNum, isDigit, isSpace, isUpper)
import qualified Data.List           (sort)
import qualified Data.Maybe          (fromJust, fromMaybe, isJust, isNothing)
---------Block1

data Expr x = Const x | Add (Expr x) (Expr x) |
    Sub (Expr x) (Expr x) | Mul (Expr x) (Expr x) |
    Div (Expr x) (Expr x) | Pow (Expr x) (Expr x) deriving (Show)

type ArithmeticError = String

evalInternal :: Expr Int -> Expr Int -> (Int -> Int -> Int) -> Either ArithmeticError Int
evalInternal x y func = eval x >>= (\f -> fmap (func f) (eval y))

-- liftA2 func (eval x) (eval y)

eval :: Expr Int -> Either ArithmeticError Int
eval (Const x) = return x
eval (Add x y) = evalInternal x y (+)
eval (Sub x y) = evalInternal x y (-)
eval (Mul x y) = evalInternal x y (*)
eval (Div x y) =
    eval x >>=
        (\f -> eval y >>=
            (\s -> if s == 0 then Left "Dividing by zero" else return $ f `div` s)) -- s != 0
eval (Pow x y) =
    eval x >>=
        (\f -> eval y >>=
            (\s -> if s < 0 then Left "Raising to the negative power" else return $ f ^ s)) -- s >= 0

data a ~> b
    = Partial   (a -> Maybe b) -- a partial function
    | Defaulted (a ~> b) b     -- a partial function with a default value

partial :: (a -> Maybe b) -> a ~> b
partial = Partial

total :: (a -> b) -> a ~> b
total f = Partial $ Just . f

apply :: (a ~> b) -> a -> Maybe b
apply (Partial f) a     = f a
apply (Defaulted f b) a = Just $ Data.Maybe.fromMaybe b (apply f a)

applyOrElse :: (a ~> b) -> a -> b -> b
applyOrElse partialFunc@(Partial _) a b = Data.Maybe.fromJust $ apply (Defaulted partialFunc b) a
applyOrElse defaulted a _ = Data.Maybe.fromJust $ apply defaulted a

withDefault :: (a ~> b) -> b -> (a ~> b)
withDefault partialFunc@(Partial _) b = Defaulted partialFunc b
withDefault (Defaulted f _) b2        = Defaulted f b2

isDefinedAt :: (a ~> b) -> a -> Bool
isDefinedAt (Partial f) a = Data.Maybe.isJust $ f a
isDefinedAt _ _           = True

orElse :: (a ~> b) -> (a ~> b) -> a ~> b
orElse f@(Defaulted _ _) _ = f
orElse f s = Partial (\x -> (apply f x) Control.Applicative.<|> (apply s x))

instance Control.Category.Category (~>) where
    id = Partial Just
    (.) f s = Partial (\l -> apply s l >>= apply f)

------------Block2

bin :: Int -> Maybe [[Int]]
bin x
    | x < 0 = Nothing
    | otherwise = Just $ binInternal (replicate x [0, 1])
  where
    binInternal [] = [[]]
    binInternal (element : xs) = element >>= (\l -> binInternal xs >>= (\l1 -> [l : l1]))


combinations :: Int -> Int -> Maybe [[Int]]
combinations x y
    | x <= 0 || y <= 0 || y > x = Nothing
    | otherwise = Just $ Data.List.sort $ combinationsInternal y $ take x [1..]
  where
    combinationsInternal _ [] = []
    combinationsInternal 1 xs = xs >>= (\l -> [[l]])
    combinationsInternal y' (element : xs) = combinationsInternal y' xs ++
        (combinationsInternal (y' - 1) xs >>= (\l -> [element : l]))

permutations :: [a] -> [[a]]
permutations [x] = [[x]]
permutations xs = permutationsInternal [] xs
  where
    permutationsInternal _ [] = []
    permutationsInternal xsint (y:ys) = (permutations (xsint ++ ys) >>= (\l -> [y : l]))
        ++ permutationsInternal (y:xsint) ys

-----------Block4

newtype Parser a
    = Parser { runParser :: String -> Maybe (a, String) }

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing -- fail on the empty input
    f (x:xs)       -- check if x satisfies the predicate
        | p x = Just (x, xs)
        | otherwise = Nothing -- otherwise, fail

char :: Char -> Parser Char
char c = satisfy (== c)

posInt :: Parser Integer
posInt = Parser f
  where
    f xs
        | null ns = Nothing
        | otherwise = Just (read ns, rest)
                        where (ns, rest) = span Data.Char.isDigit xs


applyToFst :: (a -> b) -> Maybe (a, c) -> Maybe (b, c)
applyToFst f (Just (x, y)) = Just (f x, y)
applyToFst _ Nothing       = Nothing

-- Functor laws
--     1. fmap id      ≡ id
--     2. fmap (f . g) ≡ fmap f . fmap g

instance Functor Parser where
    -- fmap f (Parser a) = Parser (\x -> a x >>= (\y -> Just (f $ fst y, snd y)))
    fmap f (Parser a) = Parser (applyToFst f . a)

-- Applicative laws
--     1. identity
--         pure id <*> v ≡ v
--     2. composition
--         pure (.) <*> u <*> v <*> w ≡ u <*> (v <*> w)
--     3. homomorphism
--         pure f <*> pure x ≡ pure (f x)
--     4. interchange
--         u <*> pure y ≡ pure ($ y) <*> u
 

instance Applicative Parser where
    pure a = Parser (\s -> Just (a, s))
    -- (<*>) (Parser f) (Parser b) = Parser (\s -> f s >>= (\y -> applyToFst (fst y) (b $ snd y)))
    (<*>) (Parser f) (Parser b) = Parser (f Control.Monad.>=> (\ y -> applyToFst (fst y) (b $ snd y)))

----For Tests begins

type Name = String
data Employee = Emp { name :: Name, phone :: String } deriving (Show, Eq)

getName :: String -> (String, String)
getName [] = ([], [])
getName (x:xsint)
    | not $ Data.Char.isDigit x = let (name_, phone_) = getName xsint in
        (x : name_, phone_)
    | otherwise = ([], x:xsint)

parseName :: Parser Name
parseName = Parser f
  where
    f [] = Nothing
    f xs = let (name_, l) = getName xs in
            if null name_
            then Nothing
            else Just (name_, l)

parsePhone :: Parser String
parsePhone = Parser f
  where
    f [] = Nothing
    f xs = if all Data.Char.isDigit xs
           then Just (xs, "")
           else Nothing

----For Tests ends

abParser :: Parser (Char, Char)
abParser = (,) <$> satisfy (== 'a') <*> satisfy (== 'b')
-- liftA2 (,) (char 'a') (char 'b')

abParser_ :: Parser ()
abParser_ = Control.Monad.void abParser

intPair :: Parser [Integer]
intPair = (\x y -> [x, y]) <$> (posInt <* satisfy (== ' ')) <*> posInt

class Applicative f => Alternative f where
    empty :: f a
    (<|>) :: f a -> f a -> f a

instance Alternative Parser where
    empty :: Parser a
    empty = Parser (const Nothing)
    (<|>) :: Parser a -> Parser a -> Parser a
    Parser a <|> Parser b = Parser (\s -> (a s) Control.Applicative.<|> (b s))

intOrUppercase :: Parser ()
intOrUppercase = (Control.Monad.void posInt) <|> (Control.Monad.void $ satisfy Data.Char.isUpper)

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p

spaces :: Parser String
spaces = zeroOrMore $ satisfy Data.Char.isSpace

ident :: Parser String
ident = (:) <$> satisfy Data.Char.isAlpha
            <*> zeroOrMore (satisfy Data.Char.isAlphaNum)


type Ident = String -- ident parser

data Atom = N Integer -- posInt parser
          | I Ident
          deriving (Show, Eq)

data SExpr = A Atom
           | Comb [SExpr]
           deriving (Show, Eq)

parseSExpr :: Parser SExpr
parseSExpr = spaces *> (parseAtom <|>
    (Comb <$> (char '(' *> zeroOrMore parseSExpr <* char ')'))) <* spaces

parseAtom :: Parser SExpr
parseAtom = ((A . I) <$> ident) <|> ((A . N) <$> posInt)


instance Monad Parser where
    return a = Parser (\s -> Just (a, s))
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    (>>=) (Parser f) g =
        Parser (\s -> let fs = f s in
                        if Data.Maybe.isNothing fs
                        then Nothing
                        else let k = g $ fst $ Data.Maybe.fromJust fs in
                            runParser k (snd $ Data.Maybe.fromJust fs))