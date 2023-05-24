import Data.Function

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

mySum :: (Integral a) => [a] -> a
mySum [] = 0
mySum (h:t) = h + mySum t

mySum2 l = case l of
    [] -> 0
    h:t -> h + mySum2 t

myMax :: (Ord a) => [a] -> a
myMax = foldl1 max

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) =
    let sm = quickSort [ t | t <- xs, t < x ]
        gr = quickSort [ t | t <- xs, t >= x ]
    in
        sm ++ [x] ++ gr


merge :: (Ord a) => [a] -> [a] -> [a]
merge [] l = l
merge l [] = l
merge (a:ax) (b:bx) =
    if a > b then
        b:merge (a:ax) bx
    else
        a:merge ax (b:bx)


mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort [e] = [e]
mergeSort l =
    merge
        (mergeSort $ take (length l `div` 2) l)
        (mergeSort $ drop (length l `div` 2) l)


listToBmis :: (RealFloat a) => [(a, a)] -> [a]
listToBmis l = [bmi w h | (w, h) <- l] where bmi w h = w / h ^ 2


-- listToBmis2 :: (Integral a) => [(a, a)] -> [a]
-- listToBmis2 = (map (*)) . (map uncurry)

bmiTell weight height
    | bmi <= skinny = "Skinny"
    | bmi <= normal = "Normal"
    | bmi <= fat = "Fat"
    | otherwise = "Whale"
    where bmi = weight / height ^ 2
          skinny = 18.5
          normal = 25.0
          fat = 30.0

main = do
    take 10 . filter (>5) . map (^3) $ [1..]
    & \(x:xs) -> reverse xs
    & print
