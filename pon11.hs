import Data.Char

suml :: IO()
suml = do
    number <- getLine
    let list = fmap (subtract 48) $ map ord number
    let result = sum list
    putStrLn $ show result
