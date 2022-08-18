n = a `div` length xs
    where
      a = 10
      xs = [1,2,3,4,5]

last1 :: [a] -> a
last1 (x:xs)
      | null xs   = x
      | otherwise = last1 xs

halve :: [a] -> ([a],[a])
halve xs = (take n xs, drop n xs)
           where n = length xs `div` 2 

thirda :: [a] -> a
thirda xs = head(tail(tail xs))

thirdb :: [a] -> a
thirdb xs = xs !! 2

safetaila :: [a] -> [a]
safetaila xs = if null xs then [] else tail xs

safetailb :: [a] -> [a]
safetailb xs | null xs = []
             | otherwise = tail xs

safetailc :: [a] -> [a]
safetailc [] = []
safetailc (_:xs) = xs

sum1 = sum[x^2 | x <- [1..100]]

replicate' :: Int -> a -> [a]
replicate' n x = [x | _ <- [1..n]]

pyths :: Int -> [(Int,Int,Int)]
pyths n = [(x,y,z) | x <- [1..n],
                     y <- [1..n],
                     z <- [1..n],
                     x^2 + y^2 == z^2]

perfects :: Int -> [Int]
perfects a = [x | x <- [1..a], sum(init (factors x)) == x]

factors :: Int -> [Int]
factors a = [x | x <- [1..a], a `mod` x == 0]






