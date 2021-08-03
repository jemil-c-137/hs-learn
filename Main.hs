factorial n =
  if n <= 1 then
    1
  else
    n * factorial (n-1)


-- guards | otherwise means === always true
fac n
  | n <= 1 = 1
  | otherwise = n * fac (n-1)

-- Accumulator

aac :: (Ord t, Num t) => t -> t
aac n = aux n 1
  where
    aux n acc
      | n <= 1 = acc
      | otherwise = aux (n-1) (n*acc)


pow2 n =
  if n == 0
    then 1
    else 2 * pow2 (n-1)


-- RECURSION
repeatString str n =
  if n == 0
    then ""
    else str ++ repeatString str (n-1)


-- LOOPS
pow3 n = pow2loop n 1 0
pow2loop n x i =
  if i < n
    then pow2loop n (x*2) (i+1)
    else x


-- LISTS
x = [0, 1, 2, 3]
b = tail x


asc :: Int -> Int -> [Int]
asc n m
  | m < n = []
  | m == n = [m]
  | m > n = n : asc (n+1) m