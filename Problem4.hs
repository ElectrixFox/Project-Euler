module Problem4 where

getDigits :: Int -> [Int]
getDigits 0 = []
getDigits x = getDigits (x `div` 10) ++ [x `mod` 10]

isPalindrome :: Int -> Bool
isPalindrome n = do
    let digs = getDigits n
    let midsplit = splitAt ((length digs + 1) `div` 2) digs
    fst midsplit == reverse (snd midsplit)


problem4 = do
    let n = 9009
    let digs = getDigits n
    let midsplit = splitAt ((length digs + 1) `div` 2) digs
    show (isPalindrome n)
    -- show midsplit