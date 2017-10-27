{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}

import qualified Prelude (id)

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

instance Monad m => MonadFish m where 
    returnFish = return
    (>=>) f g = \x -> (f x >>= g) -- f x == return x >>= f


instance Monad m => MonadJoin m where
    returnJoin = return
    join x = x >>= Prelude.id

instance Monad m => Functor f
    fmap f xs = xs >>= return . f

---Proving laws for functor
    LAW: fmap id  ==  id
fmap id ≡ (\xs -> xs >>= return . id) ≡ (f . id ≡ id . f ≡ f)
    ≡ (\xs -> xs >>= return) ≡ (\xs -> xs) ≡ id

    LAW: fmap (f . g)  ==  fmap f . fmap g
fmap f . fmap g = fmap f (fmap g) = fmap f (\xs -> xs >>= return . g)
    = (\xs -> xs >>= return (g x) >>= return . f)
    = (\xs -> xs >>= (\x -> return (g x) >>= return . f))
    = (\xs -> xs >>= (\x -> return . f (g x)))
    = (\xs -> xs >>= return . (f . g)))
    = fmap (f . g)
------------------------
MonadFish:
    LAW: f >=> returnFish ≡ f
(f >=> returnFish) a ≡ (\x -> (f x >>= returnFish)) a 
    ≡ (m >>= return ≡ m) ≡ (\x -> f x) a ≡ (β-редукция) f a

MonadJoin:
    LAW: join . fmap returnJoin ≡ id
(join . fmap returnJoin) ≡ (\x -> join (fmap returnJoin x)) 
    ≡ (\x -> join (fmap return x)) ≡ (\x -> join (x >>= return . return))
    ≡ (\x -> join (x >>= (\y -> return (return y))) ≡ (\x -> join (return x)) 
    ≡ (\x -> return x >>= Prelude.id) ≡ (\x -> Prelude.id x) ≡ (η-преобразование) = Prelude.id

    LAW: join . returnJoin ≡ id
(join . returnJoin) ≡ (\x -> join (returnJoin x)) 
    ≡ (\x -> join (return x)) ≡ (\x -> return x >>= Prelude.id) 
    ≡ (\x -> Prelude.id x) = (η-преобразование) = Prelude.id