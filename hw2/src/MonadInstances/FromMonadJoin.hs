{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}

import qualified Prelude (($))
import Data.Functor

class Monad m where
    return     :: a -> m a
    (>>=)      :: m a -> (a -> m b) -> m b

    -- {-# LAWS
    --     1. m >>= return    ≡ m
    --     2. return a >>= f  ≡ f a
    --     3. (m >>= f) >>= g ≡ m >>= (\x -> f x >>= g)
    -- #-}

class MonadFish m where
    returnFish :: a -> m a
    (>=>)      :: (a -> m b) -> (b -> m c) -> (a -> m c)

    -- {-# LAWS
    --     1. f >=> returnFish ≡ f
    --     2. returnFish >=> f ≡ f
    --     3. (f >=> g) >=> h  ≡ f >=> (g >=> h)
    -- #-}

class MonadJoin m where
    returnJoin :: a -> m a
    join       :: m (m a) -> m a

    -- {-# LAWS
    --     1. join . pure            ≡ id
    --     2. join . fmap returnJoin ≡ id
    --     3. join . fmap join       ≡ join . join
    -- #-}

instance (Functor m, MonadJoin m) => Monad m where
    return = returnJoin
    (>>=) :: m a -> (a -> m b) -> m b
    (>>=) mx fxmx = join (fmap fxmx mx)

instance (Functor m, MonadJoin m) => MonadFish m where
    returnFish = returnJoin
    (>=>) :: (a -> m b) -> (b -> m c) -> (a -> m c)
    (>=>) f g = \x -> join Prelude.$ fmap g Prelude.$ f x

-- Monad:
--     LAW: m >>= return ≡ m
-- m >>= return = (join (fmap return m)) = (join . fmap return) m = id m = m

-- --     LAW: return a >>= f  ≡ f a
-- -- return a >>= f = join (fmap f (return a))
-- --     = join ((fmap f . return) a) = 

-- MonadFish:
--     LAW: f >=> returnFish ≡ f
-- f >=> returnFish = \x -> join (fmap returnFish (f x)) 
--     = \x -> (join . fmap returnFish . f) x = \x -> ((join . fmap returnFish) . f) x = 
--     = \x -> (id . f) x = \x -> id (f x) = \x -> f x = f

-- --     LAW: returnFish >=> f ≡ f
-- -- returnFish >=> f = \x -> join (fmap f (returnFish x)) 