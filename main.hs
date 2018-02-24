import Text.Read (readMaybe)
import Text.Printf (printf)
import System.IO (hFlush, stdout)
import Data.Char (ord)

type Binary = [Int]
type Hex = [Char]

main = do
    printf "Input the ASCII text to be converted.\n"
    hFlush stdout
    text <- getLine
    printf "Binary: %s\n" $ unwords $ map (concat . concat) 
        $ map (fmap $ map show) $ map toBin text
    printf "Hexadecimal: %s\n" $ unwords $ map concat $ map toHex text
    -- pause
    pause <- getLine
    return ()


toBin :: Char -> Maybe Binary
toBin x = case x of
    n | ord n `elem` [0..255] -> do
        let loop (x, r) = do
            case (x, r) of
                (0, r) -> return [r]
                (x, r) -> do
                    state <- loop $ x `divMod` 2 
                    return (state ++ [r])
        result <- loop $ ord n `divMod` 2
        let diff = 8 - length result 
        -- expands the result to be exactly 8-long
        return $ (concat $ replicate diff [0]) ++ result
    _ -> Nothing

toHex :: Char -> Maybe Hex
toHex x = case x of
    n | ord n `elem` [0..255] -> do
        let loop (x, r) = do
            case (x, r) of
                (0, r) -> return [inHex r]
                (x, r) -> do
                    state <- loop $ x `divMod` 16
                    return (state ++ [inHex r])
        result <- loop $ ord n `divMod` 16
        return $ if length result /= 2
            then '0' : result 
            else result
        
    _ -> Nothing

inHex :: Int -> Char
inHex n = case n of
    0  -> '0'
    1  -> '1'
    2  -> '2'
    3  -> '3'
    4  -> '4'
    5  -> '5'
    6  -> '6'
    7  -> '7'
    8  -> '8'
    9  -> '9'
    10 -> 'a'
    11 -> 'b'
    12 -> 'c'
    13 -> 'd'
    14 -> 'e'
    15 -> 'f'
    _  -> '?'