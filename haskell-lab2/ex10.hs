concatNew :: [[a]] -> [a]
concatNew tab = [y | x <- tab, y <- x ]
