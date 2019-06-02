main = do
    putStrLn "Podaj imię: "
    name <- getLine
    putStrLn "Podaj wiek: "
    ageStr <- getLine
    let age = read ageStr::Int
    if age < 12
       then putStrLn ("HAHHA") 
       else putStrLn ("HEHEH")
