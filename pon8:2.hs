triple :: [a] -> [a]
triple [] = []
triple (h:t) = (replicate 3 h) ++ triple t