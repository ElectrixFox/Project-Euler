module Problem1 where

isMult p n = if n `mod` p == 0 then n else 0

problem1 = do
    let rang = [1..1000-1]
    let res = map (isMult 3) rang ++ map (isMult 5) rang
    sum res