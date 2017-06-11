data Drzewo a = Wezel a [Drzewo a] 

lwierz :: Drzewo a -> [a]
lwierz (Wezel a []) = [a]
lwierz (Wezel a t:ts) = a : (foldr)

