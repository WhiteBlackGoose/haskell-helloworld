import Data.Function
import Heh (sphereVolume)

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

mySum :: (Integral a) => [a] -> a
mySum [] = 0
mySum (h:t) = h + mySum t

mySum2 l = case l of
    [] -> 0
    h:t -> h + mySum2 t

myMax :: (Num a) => [a] -> a
myMax = foldl1 (+)

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

data Frog = Frog {
        aaa :: Int,
        bbb :: [Char]
    } deriving (Show)

class Comp a where
    (>--) :: a -> a -> Bool

data TrafficLight = Green | Yellow | Red
instance Comp TrafficLight where
    Green >-- Green = False
    Yellow >-- Yellow = False
    Red >-- Red = False
    Green >-- Red = False
    Green >-- Yellow = False
    Yellow >-- Green = True
    Yellow >-- Red = False
    Red >-- Green = True
    Red >-- Yellow = True

data MyMaybe a = Some a | None deriving Show

instance (Eq m) => Eq (MyMaybe m) where
    Some x == Some y = y == x 
    None == None = True
    _ == _ = False

mmap :: (a -> b) -> MyMaybe a -> MyMaybe b
mmap _ None = None
mmap func (Some a) = (Some . func) a

data MyList a = Head a (MyList a) | Empty

instance Functor MyList where
    fmap m c = map m c

main = do
    let aaa :: MyMaybe Int = Some 5
    print aaa
    let bbb = mmap (show . (*2)) aaa
    print bbb
    print $ Green >-- Yellow
