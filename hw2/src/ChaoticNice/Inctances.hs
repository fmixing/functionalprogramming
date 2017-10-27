{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}

import qualified Prelude (Show, Eq, ($))
import Data.Functor 

-- Functor
-- Applicative
-- Foldable
-- Traversable


-- Identity
-- Either a b
-- Tree a  -- ваше дерево из предыдущего задания
-- Const a b
-- (a, b)

newtype Identity a = Identity { runIdentity :: a } deriving (Prelude.Show, Prelude.Eq)

newtype Const a b = Const { getConst :: a } deriving (Prelude.Show, Prelude.Eq)

data Tree a = Leaf | Node a (Tree a) (Tree a) deriving (Prelude.Show)

data Either a b  =  Left a | Right b deriving (Prelude.Eq, Prelude.Show)

data Pair a b = Pair a b

-- Functor :: * -> *

instance Functor Identity where
    fmap f (Identity x) = Identity Prelude.$ f x

instance Functor (Either e) where {- Either a :: * -> * -}
    fmap _ (Left e) = Left e
    fmap f (Right a) = Right (f a)

instance Functor Tree where
    fmap _ Leaf = Leaf
    fmap f (Node a treeL treeR) = Node (f a) (fmap f treeL) (fmap f treeR)

instance Functor (Const a) where
    fmap _ (Const x) = (Const x)

instance Functor (Pair a) where
    fmap f (Pair x y) = Pair x (f y)

-- Applicative