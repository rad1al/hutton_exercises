import Data.Char
import Data.List (sort)

{- Chapter 7.9 Exercises -}

{- 1. 

[f x | x <- xs, p x] can be re-expressed as: 

map_and_filter f p = map f . filter p

-}

{- 2. 

Errata: type definition of all and any should be
    (a -> Bool) -> [a] -> Bool 
instead of:
    (a -> Bool) -> [Bool] -> Bool

-}

all' :: (a -> Bool) -> [a] -> Bool
all' p = and . map p 

any' :: (a -> Bool) -> [a] -> Bool
any' p = or . map p

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs) | p x       = x : takeWhile' p xs
                    | otherwise = []  

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p (x:xs) | p x = dropWhile' p xs
                    | otherwise = x : xs

{- 

dropWhile' (< 3) [1,2,3,4,5]
= dropWhile (<3) [2,3,4,5]
= dropWhile (<3) [3,4,5]
= dropWhile (<3) [4,5]
= 4 : [5]
= [4,5]

-}


{- 3. -}

-- map''' :: (a -> b) -> [a] -> [b]
-- map''' f [] = []
-- map''' f (x:xs) = f x : map'' f xs

map''' :: (a -> b) -> [a] -> [b]
map''' f = foldr (\x xs -> (f x):xs) []


-- filter'' :: (a -> Bool) -> [a] -> [a]
-- filter'' p xs = [x | x <- xs, p x]

filter''' :: (a -> Bool) -> [a] -> [a]
filter''' p = foldr (\x xs -> if (p x) then x:xs else xs) []


{- 4. -}

dec2int :: [Int] -> Int
dec2int = foldl (\x y -> 10*x + y) 0

{- 5. -}

curry' :: ((a,b) -> c) -> (a -> b -> c)
curry' f = \x y -> f (x,y)

uncurry' :: (a -> b -> c) -> ((a,b) -> c)
uncurry' f = \(x,y) -> f x y

{- 6. -}
unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b] 
unfold p h t x | p x = []
               | otherwise = h x : unfold p h t (t x)

{- 

int2bin  13        == [1,0,1,1]

int2bin' :: Int -> [Bit]
int2bin' = unfold (== 0) (`mod` 2) (`div` 2)

int2bin' 13
= [1,0,1,1]

-}

type Bit = Int

chop8' :: [Bit] -> [[Bit]]
chop8' = unfold null (take 8) (drop 8)

map'''' :: (a -> b) -> [a] -> [b]
map'''' f = unfold null (f . head) tail

iterate' :: (a -> a) -> a -> [a]
iterate' f = unfold (const False) id f

{-

chop8' [1,0,0,0,0,1,1,0,0,1,0,0,0,1,1,0,1,1,0,0,0,1,1,0]
= (unfold null (take 8) (drop 8)) [1,0,0,0,0,1,1,0,0,1,0,0,0,1,1,0,1,1,0,0,0,1,1,0]
= [1,0,0,0,0,1,1,0] : (unfold null (take 8) (drop 8)) (drop 8 [0,1,0,0,0,1,1,0,1,1,0,0,0,1,1,0])
= [1,0,0,0,0,1,1,0] : [0,1,0,0,0,1,1,0] : (unfold null (take 8) (drop 8)) (drop 8 [1,1,0,0,0,1,1,0])
= [1,0,0,0,0,1,1,0] : [0,1,0,0,0,1,1,0] : [1,1,0,0,0,1,1,0] : (unfold null (take 8) (drop 8)) (drop 8 [])
= [1,0,0,0,0,1,1,0] : [0,1,0,0,0,1,1,0] : [1,1,0,0,0,1,1,0] : [])
= [[1,0,0,0,0,1,1,0],[0,1,0,0,0,1,1,0],[1,1,0,0,0,1,1,0]]


(map'''' even) [1,2,3,4,5]
= (unfold null (even . head) tail) [1,2,3,4,5]
= (even 1) : unfold null (even . head) tail [2,3,4,5]
= (even 1) : (even 2) : unfold null (even . head) tail [3,4,5]
= (even 1) : (even 2) : (even 3) : unfold null (even . head) tail [4,5]
= (even 1) : (even 2) : (even 3) : (even 4) : unfold null (even . head) tail [5]
= (even 1) : (even 2) : (even 3) : (even 4) : (even 5) : unfold null (even . head) tail []
= (even 1) : (even 2) : (even 3) : (even 4) : (even 5) : []
= False : True : False : True : False
= [False,True,False,True,False]

iterate (*2) 1
= unfold (const False) id (*2) 1
= 1 : unfold (const False) id (*2) 1*2
= 1 : 2 : unfold (const False) id (*2) 2*2
= 1 : 2 : 4 : unfold (const False) id (*2) 4*2

= [1,2,4,8,16..]

-}


{- 7. -}

bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2*y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode' :: String -> [Bit]
encode' = concat . map (addParityBit . make8 . int2bin . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

chop9 :: [Bit] -> [[Bit]]
chop9 [] = []
chop9 bits = take 9 bits : chop9 (drop 9 bits)

getParity :: [Bit] -> Bit
getParity bits = (sum bits) `mod` 2

addParityBit :: [Bit] -> [Bit]
addParityBit xs = [if odd . sum $ xs then 1 else 0] ++ xs 

checkParityBit :: [Bit] -> [Bit]
checkParityBit (x:xs) | x == getParity xs = xs
                      | otherwise = error "parity error"  
decode :: [Bit] -> String
decode = map (chr . bin2int . checkParityBit) . chop9

channel :: [Bit] -> [Bit]
channel = id

transmit :: String -> String
transmit = decode . channel . encode'

{- 8. -}

noisy_channel :: [Bit] -> [Bit]
noisy_channel bits = tail bits

noisy_transmit :: String -> String
noisy_transmit = decode . noisy_channel . encode'

{- 

transmit "higher-order functions are easy"
> "higher-order functions are easy

noisy_transmit "higher-order functions are easy"
> "*** Exception: parity error
CallStack (from HasCallStack):
  error, called at ch7.hs:184:37 in main:Main

-}

{- 9. -}

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g []     = []
altMap f g (x:[]) = f x : []
altMap f g (x:y:xs) = f x : g y : altMap f g xs 

{- 

altMap (+10) (+100) [0,1,2,3,4]
= 10 : 100 : (altMap (+10) (+100) [2,3,4])
= 10 : 100 : (12 : 102 : (altMap (+10) (+100) [4]))
= 10 : 100 : (12 : 102 : (14 : []))
= [10,100,12,102,14]

-}

{- 10. -}


luhnDouble :: Int -> Int
luhnDouble x | 9 < (x * 2) = (x * 2) - 9
             | otherwise   = (x * 2)

luhnAlt :: [Int] -> Bool
luhnAlt xs = (sum $ altMap (luhnDouble) id xs) `mod` 10 == 0

{- 

luhnAlt [1,7,8,4]
= True

luhnAlt [4,7,8,3]
= False

-}





