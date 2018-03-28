module H99 where

{-
> dupli [1, 2, 3]
[1,1,2,2,3,3]
-}
dupli :: [a] -> [a]
dupli = concatMap (take 2 . repeat)
-- take n . repeat = replicate n

{-
> repli "abc" 3
"aaabbbccc"
-}
repli :: [a] -> Int -> [a]
repli x n = concatMap (replicate n) x

{-
dropEvery "abcdefghik" 3
"abdeghk"
-}
dropEvery :: [a] -> Int -> [a]
dropEvery x n =
  map snd $ filter (\(x, _) -> x `mod` n /= 0) $ zip [1 ..] x

dropEvery' :: [a] -> Int -> [a]
dropEvery' x n = [c | (index, c) <- zip [1 ..] x, index `mod` n /= 0]

{-
split "abcdefghik" 3
("abc", "defghik")
-}
split :: [a] -> Int -> ([a], [a])
split l n = (take n l, drop n l)

split' :: [a] -> Int -> ([a], [a])
split' l n = go l n []
  where
    go l'' 0 l' = (reverse l', l'')
    go (x:xs) n' l' = go xs (n' - 1) (x : l')

{-
slice ['a','b','c','d','e','f','g','h','i','k'] 3 7
"cdefg"
-}
slice :: [a] -> Int -> Int -> [a]
slice l start end = [b | (a, b) <- zip [1 ..] l, a >= start, a <= end]

{-
*Main> rotate ['a','b','c','d','e','f','g','h'] 3
"defghabc"

*Main> rotate ['a','b','c','d','e','f','g','h'] (-2)
"ghabcdef"
-}
rotate :: [a] -> Int -> [a]
rotate l n
  | n < 0 = go l (length l + n)
  | otherwise = go l n
  where
    go l' n' = [b | (a, b) <- zip [0 ..] l', a >= n'] ++ take n' l'

rotate' :: [a] -> Int -> [a]
rotate' l n
  | n < 0 = go l (length l + n)
  | otherwise = go l n
  where
    go l' n' = drop n' l' ++ take n' l'

rotate'' :: [a] -> Int -> [a]
rotate'' l n
  | n < 0 = go l (length l + n) []
  | otherwise = go l n []
  where
    go xs 0 l'' = xs ++ reverse l''
    go (x:xs) n' l'' = go xs (n' - 1) (x : l'')

{-
*Main> removeAt 2 "abcd"
('b',"acd")
-}
removeAt :: Int -> [a] -> (a, [a])
removeAt n l = go n l []
  where
    go 1 (x:xs) l' = (x, reverse l' ++ xs)
    go n (x:xs) l' = go (n - 1) xs (x:l')
