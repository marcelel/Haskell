{-# LANGUAGE InstanceSigs #-}
data Problem a = Ok a | Error String deriving Show

instance Functor Problem where
    fmap :: (a -> b) -> Problem a -> Problem b
    fmap f (Error a) = Error a
    fmap f (Ok a) = Ok $ f a

instance Applicative Problem where
    pure a = Ok a
    (<*>) :: Problem (a -> b) -> Problem a -> Problem b
    Error f <*> problem = Error f
    Ok f <*> problem = fmap f problem

instance Monad Problem where
    return a = Ok a
    (>>=) :: Problem a -> (a -> Problem b) -> Problem b 
    Error a >>= f = Error a
    Ok a >>= f = f a