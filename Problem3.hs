module Problem3 where

getNextSet :: [Int] -> [Int]
getNextSet ps = do
    let e = last ps
    let isPrime n = all (\p -> n `mod` p /= 0) ps 
    let nums = [2..e ^ 2]   -- gets all the numbers up to the last prime squared
    let new = takeWhile (\p -> p <= e ^ 2) $ filter isPrime nums -- filters out all of the numbers who's minimum is 0 (this indicated they divide something)
    ps ++ new

getPrimesUpTo :: Int -> [Int] -> [Int]
getPrimesUpTo n ps
    | maximum ps >= n = filter (<= n) ps
    | otherwise = getPrimesUpTo n (getNextSet ps)

findSmallestFact :: Int -> [Int] -> Int
findSmallestFact n ps
    | null ps = n
    | n `mod` head ps == 0 = head ps
    | otherwise = findSmallestFact n (tail ps)

factorise :: Int -> [Int] -> [Int]
factorise n ps
    | n `elem` ps = [n]
    | otherwise = sfac : factorise (n `div` sfac) ps
    where
        sfac = findSmallestFact n ps


isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

problem3 = do
    let n = 600851475143 -- 87625999 -- 600851475143
    let prims = getPrimesUpTo (isqrt n) [2, 3]
    show (maximum (factorise n prims))