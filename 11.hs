data Tree a = Node a [Tree a] | Empty deriving Show

fun :: Int -> Int -> a -> Tree a
fun 0 _ value = Node value []
fun depth branch value = Node value (replicate branch (Node value [fun (depth - 1) branch value]))
