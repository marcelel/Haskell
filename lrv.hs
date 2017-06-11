data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a)

instance Show Tree where
    show Empty = "Empty"
    show (Leaf a) = show a
    show (Node l val r) = show val

    lrv :: Tree a -> a
    lrv (Leaf leaf) = leaf
    lrv Empty = Empty
    lrv (Node left val Empty) = lrv left ++ val
    lrv (Node Empty val right) = lrv right ++ val
    lrv (Node left val right) = lrv left ++ (lrv right) ++ (show val)