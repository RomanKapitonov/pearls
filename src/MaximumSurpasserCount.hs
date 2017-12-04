module MaximumSurpasserCount (
    msc1
  , msc2
  , msc3) where

-- Example:
-- ["telgrapher","elgrapher","lgrapher","grapher","rapher","apher","pher","her","er","r"]
tails [] = []
tails (x:xs) = (x:xs):(tails xs)

msc1 :: Ord a => [a] -> Int
msc1 xs = maximum [scount z zs | z:zs <- tails xs]

scount :: Ord a => a -> [a] -> Int
scount x xs = length (filter (x<) xs)

msc2 :: Ord a => [a] -> Int
msc2 = maximum . map snd . table1

table1 :: Ord a => [a] -> [(a, Int)]
table1 xs = [(z, scount z zs) | z:zs <- tails xs]

testData :: [Char]
testData = ['t', 'e', 'l', 'g', 'r', 'a', 'p', 'h', 'e', 'r']

-- table (xs ++ ys) = join (table xs) (table ys)
-- tails (xs ++ ys) = map (++ ys) (tails xs) ++ tails ys

-- tails (xs ++ ys) ==
-- [(z, scount z zs) | z:zs <- tails(xs ++ ys)] ==
-- [(z, scount z zs) | z:zs <- map (++ ys) (tails xs) ++ (tails ys)] ==
-- Using distribution of <- over ++
-- [(z, scount z (zs ++ ys)) | z:zs <- tails xs ] ++ [(z, scount z zs) | z:zs <- tails ys] ==
-- since: scount z (zs ++ ys) == scount z zs + scount z ys =>
-- [(z, scount z zs + scount z ys) | z:zs <- tails xs] ++ [(z, scount z zs) | z:zs <- tails ys]
-- using definition of table and ys = map fst (table ys) =>
-- [(z, c + scount z (map fst (table ys))) | (z, c) <- table xs] ++ table ys

table2 :: Ord t => [t] -> [(t, Int)]
table2 [x] = [(x, 0)]
table2 xs = join1 (table2 ys) (table2 zs)
            where m        = length xs
                  n        = m `div` 2
                  (ys, zs) = splitAt n xs

join1 :: Ord a => [(a, Int)] -> [(a, Int)] -> [(a, Int)]
join1 txs tys = [(z, c + tcount1 z tys) | (z, c) <- txs] ++ tys

tcount1 :: Ord a => a -> [(a, b)] -> Int
tcount1 z tys = scount z (map fst tys)

-- tcount z tys ==
-- length (filter (<z) (map fst tys))
-- since: filter p . map f == map f . filter (p . f)
-- length (map fst (filter ((<z) . fst)) tys)
-- since: length length . map f == length
-- length (filter ((<z) . fst) tys)
-- since: tys is ordered by fst
-- length (dropWhile ((z >=) . fst) tys)

tcount2 :: Ord a => a -> [(a, b)] -> Int
tcount2 z tys = length (dropWhile ((z >=) . fst) tys)

table3 :: Ord t => [t] -> [(t, Int)]
table3 [x] = [(x, 0)]
table3 xs = join2 (m - n) (table3 ys) (table3 zs)
            where m = length xs
                  n = m `div` 2
                  (ys, zs) = splitAt n xs

join2 :: (Ord t, Num a) => a -> [(t, a)] -> [(t, a)] -> [(t, a)]
join2 n txs [] = txs
join2 n [] tys = tys
join2 n txs@((x, c):txs') tys@((y, d):tys')
  | x < y = (x, c + n):join2 n txs' tys
  | otherwise = (y, d):join2 (n - 1) txs tys'

msc3 :: Ord a => [a] -> Int
msc3 = maximum . map snd . table3