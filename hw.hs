import Data.Function

factorial :: (Integral a) => a -> a  
factorial 0 = 1  
factorial n = n * factorial (n - 1)  

mySum :: (Integral a) => [a] -> a
mySum [] = 0
mySum (h:t) = h + mySum t

main = do
    mySum [1, 2, factorial 4]
    & print
