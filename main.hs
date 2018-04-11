main = do
    putStrLn "Input the ASCII text to be converted."
    text <- getLine
    putStrLn $ unwords . ("Binary: " :) . map toBin $ text
    putStrLn $ unwords . ("Hexadecimal: " :) . map toHex $ text
    -- pause
    getLine >> return ()

toNary :: Int -> Int -> Int ->  [Int]
toNary prec n =
  reverse . take prec . map (flip mod n) . iterate (flip div n)

toBin :: Char -> String
toBin = map showDigit . toNary 8 2 . fromEnum

toHex :: Char -> String
toHex = map showDigit . toNary 2 16 . fromEnum

showDigit :: Int -> Char
showDigit n =
  let digits = ['0'..'9'] ++ ['a'..'z']
  in if n >= 0 && n < length digits
     then digits !! n
     else '?'
