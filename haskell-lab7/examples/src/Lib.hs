module Lib
    ( run
    , split
    , unsplit
    ) where

import Data.List (intercalate)

unsplit :: Char -> [String] -> String
unsplit c = intercalate [c]

split :: Char -> String -> [String]
split c [] = []
split c xs = xs' : if null xs'' then [] else split c (tail xs'')
    where xs' = takeWhile (/=c) xs
          xs''= dropWhile (/=c) xs

examples = [('@',"abc@def.pl"), ('/',"/usr/include")]

test (c,xs) = unwords ["split", show c, show xs, "=", show ys]
    where ys = split c xs

run :: IO ()
run = mapM_ (putStrLn.test) examples