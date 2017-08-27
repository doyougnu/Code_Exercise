module P3 where

-- The prime factors of 13195 are 5, 7, 13 and 29.

-- What is the largest prime factor of the number 600851475143 ?

pf :: Integer -> [Integer]
pf 1 = []
pf n
  | even n = 2 : pf (div n 2)
  | otherwise = go 3 n
    where
      go i n
        | mod n i == 0 = i : pf (div n i)
        | otherwise = go (succ i) n

answer = maximum $ pf 600851475143

-- Timing
-- pf 600851475143
-- [71,839,1471,6857]
-- (0.03 secs, 3646784 bytes)

-- We could also ignore the list and just keep the last value, it will always be
-- the maximum but I find the list method more haskelly

-- we could also probably use unfold or scanl for this
