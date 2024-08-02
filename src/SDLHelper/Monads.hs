module SDLHelper.Monads where
    
ifM :: (Monad m) => m Bool -> a -> a -> m a
ifM cond x y = cond >>= \b -> if b then pure x else pure y