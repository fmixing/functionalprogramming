{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs #-}

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
       ) where

import           Data.Maybe          (fromJust, isNothing, fromMaybe)
import qualified Control.Category 
import Data.List (sortBy)
import qualified Control.Applicative ((<|>))
import Control.Monad (void)
import qualified Data.Char (isDigit, isUpper)
---------Block1

data Expr x = Const x | Add (Expr x) (Expr x) |
    Sub (Expr x) (Expr x) | Mul (Expr x) (Expr x) |
    Div (Expr x) (Expr x) | Pow (Expr x) (Expr x) deriving (Show)

type ArithmeticError = String

evalInternal :: Expr Int -> Expr Int -> (Int -> Int -> Int) -> Either ArithmeticError Int
evalInternal x y func = eval x >>= (\f -> eval y >>= (\s -> return $ func f s))

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
partial f = Partial f

total :: (a -> b) -> a ~> b
total f = Partial $ Just . f

apply :: (a ~> b) -> a -> Maybe b
apply (Partial f) a = f a
apply (Defaulted f b) a = Just $ fromMaybe b (apply f a) 

applyOrElse :: (a ~> b) -> a -> b -> b
applyOrElse partialFunc@(Partial _) a b = fromJust $ apply (Defaulted partialFunc b) a
applyOrElse defaulted a _ = fromJust $ apply defaulted a

withDefault :: (a ~> b) -> b -> (a ~> b)
withDefault partialFunc@(Partial _) b = Defaulted partialFunc b
withDefault (Defaulted f _) b2 = Defaulted f b2

isDefinedAt :: (a ~> b) -> a -> Bool
isDefinedAt (Partial f) a = isNothing $ f a
isDefinedAt (Defaulted f _) a = isDefinedAt f a
isDefinedAt _ _ = True

--todo
-- orElse :: (a ~> b) -> (a ~> b) -> a ~> b
-- orElse f s = Partial internalFunc
--   where
--     internalFunc x = let fres = apply f x in 
--         if isNothing fres 
--         then apply s x
--         else fres

orElse :: (a ~> b) -> (a ~> b) -> a ~> b
orElse f@(Defaulted _ _) _ = f
orElse f s = Partial (\x -> (apply f x) Control.Applicative.<|> (apply s x))

instance Control.Category.Category (~>) where
    id = Partial $ Just . Prelude.id
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
    | x <= 0 || y <= 0  || y > x = Nothing
    | otherwise = Just $ sortBy compare $ combinationsInternal y $ take x [1..]
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
        ++ (permutationsInternal (xsint ++ [y]) ys)

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
applyToFst _ Nothing = Nothing


instance Functor Parser where
    -- fmap f (Parser a) = Parser (\x -> a x >>= (\y -> Just (f $ fst y, snd y)))
    fmap f (Parser a) = Parser $ (\x -> applyToFst f (a x))

instance Applicative Parser where
    pure a = Parser $ (\s -> Just (a, s))
    (<*>) (Parser f) (Parser b) = Parser $ (\s -> f s >>= (\y -> applyToFst (fst y) (b $ snd y)))

----For Tests begins

type Name = String
data Employee = Emp { name :: Name, phone :: String } deriving (Show)

getName :: String -> (String, String)
getName [] = ([], [])
getName (x:xsint)
    | not $ Data.Char.isDigit x = let (name_, phone_) = getName xsint in 
        ([x] ++ name_, phone_)
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

-- let k = Emp <$> parseName <*> parsePhone
-- runParser k "Alice123"

----For Tests ends

abParser :: Parser (Char, Char)
abParser = (,) <$> satisfy (== 'a') <*> satisfy (== 'b')

abParser_ :: Parser ()
abParser_ = void abParser

-- k = (,) <$> aParser <*> bParser
-- runParser k "abc"
-- Just (('a','b'),"c")
-- runParser k "bc"
-- Nothing
-- runParser k "ac"
-- runParser abParser_ "abc"
-- Just (('a','b'),"c")

intPair :: Parser [Integer]
intPair = (\x y -> [x, y]) <$> (posInt <* satisfy (== ' ')) <*> posInt

class Applicative f => Alternative f where
    empty :: f a
    (<|>) :: f a -> f a -> f a

instance Alternative Parser where
    empty :: Parser a
    empty = Parser (\_ -> Nothing)
    (<|>) :: Parser a -> Parser a -> Parser a
    (Parser a) <|> (Parser b) = Parser $ (\s -> (a s) Control.Applicative.<|> (b s))

intOrUppercase :: Parser ()
intOrUppercase = (void posInt) <|> (void $ satisfy (Data.Char.isUpper))

-- runParser intOrUppercase "123abc"
-- Just ((),"abc")

-- runParser intOrUppercase "ABC"
-- Just ((),"BC")

-- runParser intOrUppercase "abc"
-- Nothing