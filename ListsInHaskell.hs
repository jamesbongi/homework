{-- Chapter 5 --}

{-- 1. --}
sumsquares n = sum [x^2 | x <- [0..n]]

{-- 2. --}
replicate' n v = [v | _ <- [1..n]]

{-- Chapter 6 --}

{--
2. 
length [1, 2, 3]
= {applying length}
1 + length [2, 3]
= {applying length}
1 + (1 + length [3])
= {applying length}
1 + (1 + (1 + length []))
= {applying length}
1 + (1 + (1 + 0))
= { applying + }
3
--}

{-- Chapter 7 --}

{-- 1 --}

mapterhension :: (a -> b) -> (a -> Bool) -> [a] -> [b]
mapterhension f p = map f . filter p

{-- 2 --}

all' :: (a -> Bool) -> [a] -> Bool
all' _ [] = True
all' f (x:xs) = f x && all' f xs

any' :: (a -> Bool) -> [a] -> Bool
any' _ [] = False
any' f (x:xs) = f x || any' f xs
