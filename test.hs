dot :: (b -> c) -> (a -> b) -> a -> c
dot f g a = f $ g a
infixr 9 `dot`

qwe = do
    line <- getLine
    putStrLn $ reverse line

qwe' = do
    x <- getLine
    y <- getLine
    z <- getLine
    return $ reverse z ++ " " ++ reverse y ++ " " ++ reverse x