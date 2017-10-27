{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}

import qualified Prelude (id, const, undefined)

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
    --     1. join . returnJoin      ≡ id
    --     2. join . fmap returnJoin ≡ id
    --     3. join . fmap join       ≡ join . join
    -- #-}

instance MonadFish m => Monad m where
    return = returnFish
    (>>=) mx fxmy = (Prelude.id >=> fxmy) mx
    -- (>>=) mx fxmy = ((\x -> Prelude.const mx x) >=> fxmy) Prelude.undefined

instance MonadFish m => MonadJoin m where
    returnJoin = returnFish
    join :: m (m a) -> m a
    join = Prelude.id >=> Prelude.id

instance MonadFish m => Functor f
    fmap f xs = (id >=> (returnFish . f)) xs

---Proving laws for functor
    LAW: fmap id  ==  id
fmap id ≡ (\xs -> (id >=> (returnFish . id)) xs) ≡ (\xs -> (id >=> returnFish) xs)
    ≡ (\xs -> id xs) ≡ id

    LAW: fmap (f . g)  ==  fmap f . fmap g
fmap f . fmap g = fmap f (fmap g) = fmap f (\xs -> (id >=> (returnFish . g)) xs)
    = (\xs -> ((id >=> (returnFish . f)) . (id >=> (returnFish . g))) xs)
    = (\xs -> (( >=> (returnFish . f)) . id . (id >=> (returnFish . g))) xs)
    = (\xs -> (( >=> (returnFish . f)) . (id >=> (returnFish . g))) xs)
    = (\xs -> ((id >=> (returnFish . g)) >=> (returnFish . f)) xs)
    = (\xs -> ((id >=> ((returnFish . g) >=> (returnFish . f))) xs)
    = (\xs -> ((id >=> ( >=> (returnFish . f)) . returnFish . g) xs)
    = (\xs -> ((id >=> (returnFish >=> (returnFish . f)) . g) xs)
    = (\xs -> ((id >=> ((returnFish . f)) . g) xs)
    = (\xs -> (id >=> (returnFish . (f . g)) xs)
    = fmap (f . g)
--------------------

fmap f . fmap g = fmap f (fmap g) = fmap f (\xs -> xs >>= return . g)
    = (\xs -> xs >>= return (g x) >>= return . f)
    = (\xs -> xs >>= (\x -> return (g x) >>= return . f))
    = (\xs -> xs >>= (\x -> return . f (g x)))
    = (\xs -> xs >>= return . (f . g)))
    = fmap (f . g)


Monad 
    LAW: m >>= return ≡ m
m >>= return = (Prelude.id >=> returnFish) m = Prelude.id m = m

    LAW: return a >>= f  ≡ f a
return a >>= f = returnFish a >>= f = (Prelude.id >=> f) returnFish a
    = ((\x -> Prelude.id x) >=> (\y -> f y)) returnFish a
    = ((Prelude.id >=> f) . returnFish) a = (( >=> f) . Prelude.id . returnFish) a
    = (( >=> f) . (Prelude.id . returnFish)) a = (f . id ≡ id . f ≡ f) =
    = (( >=> f) . returnFish) a = (returnFish >=> f) a = f a

MonadJoin:
    LAW: join . returnJoin ≡ id

(Prelude.id >=> Prelude.id) . returnJoin = (( >=> Prelude.id) . Prelude.id) . returnJoin
    = (( >=> Prelude.id) . Prelude.id) . returnJoin = ( >=> Prelude.id) . returnJoin
    = (returnJoin >=> Prelude.id) = Prelude.id

