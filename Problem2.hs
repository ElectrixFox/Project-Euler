module Problem2 where

fib 1 = 1
fib 2 = 2
fib n = fib (n - 1) + fib (n - 2)

getEvenFibs :: Int -> Int -> [Int]
getEvenFibs x n
    | fib x > n = []
    | odd (fib x) = getEvenFibs (x + 1) n
    | otherwise = fib x : getEvenFibs (x + 1) n

problem2 = do
    sum (getEvenFibs 1 4000000)
