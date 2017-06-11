($$) :: (a->b) -> a -> b
($$) f x = f x

foo :: IO ()
foo = do 
    a <- getLine
    b <- getLine
    c <- getLine
    putStrLn $ reverse c ++ " " ++ reverse b ++ " " ++ reverse a

-- newtype Identity a = Identity { runIdentity :: a }
-- instance Monad Identity where
-- return a = Identity a -- i.e. return = id
-- (Identity x) >>= f = f x -- i.e. x >>= f = f x

op :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
op m n x = do
    tmp <- m x
    n tmp