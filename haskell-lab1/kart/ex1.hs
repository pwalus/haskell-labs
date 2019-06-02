notLike :: Int -> Bool
notLike 1 = True
notLike _ = False

id :: a -> a
id x = x

printHello = putStrLn "Hello"
main = printHello
