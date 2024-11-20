pyths :: Int -> [(Int,Int,Int)]
pyths n = [(x,y,n) | x <- [1..n], y <- [1..n], x^2 + y^2 == n^2]

divisors :: Int -> [Int]
divisors n = [x | x <- [1..n-1], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], sum(divisors x) == x]

abundants :: Int -> [Int]
abundants n = [x | x <- [1..n], sum(divisors x) < x]

deficients :: Int -> [Int]
deficients n = [x | x <- [1..n], sum(divisors x) > x]

scalprod :: [Int] -> [Int] -> Int
scalprod xs ys = sum [x * y | (x, y) <- zip xs ys]

and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && and' xs

concat' :: [[a]] -> [a]
concat' [] = []
concat' (xs:xss) = xs ++ concat' xss

replicate' :: Int -> a -> [a]
replicate' 0 x = []
replicate' n x = x : replicate' (n-1) x

(!!!) :: [a] -> Int -> a
(!!!) (x:xs) 0 = x
(!!!) (x:xs) n = (!!!) xs (n-1)

elem' :: Eq a => a -> [a] -> Bool
elem' n [] = False
elem' n (x:xs) = if n == x then True else elem n xs

fun :: Num a => (a -> a) -> (a -> Bool) -> [a] -> [a]
fun f p (x:xs) = map f (filter p xs)

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (:) f xs 