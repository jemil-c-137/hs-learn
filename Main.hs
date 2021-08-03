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

-- functions on lists
b = tail x -- return a tail of lise // [1,2,3]
c = head x -- return first element // 0
d = head [1,2,3,4,5] -- 1

-- Function composition example 
headOfTail = head(tail x)

asc :: Int -> Int -> [Int]
asc n m
  | m < n = []
  | m == n = [m]
  | m > n = n : asc (n+1) m

-- doubles numbers in the list // double x == [0,2,4,6]
double nums =
  if null nums
    then []
    else 2 * head nums : double (tail nums)


removeOdd numbers =
  if null numbers
    then []
    else 
      if (mod (head numbers) 2) == 0 --even ?
      then head numbers : removeOdd (tail numbers)
      else removeOdd (tail numbers)

-- another syntax
removeOdd2 numbers
  | null numbers = []
  | mod (head numbers) 2 == 0 = head numbers : removeOdd2 (tail numbers)
  | otherwise = removeOdd2 (tail numbers)