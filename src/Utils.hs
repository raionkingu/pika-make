module Utils ( isLeft, fromRight
             , rtrim
             , count, countWith
             , map1, map2, mapL, mapR
             ) where

import Data.Char (isSpace)

--  Believe me, this is not implemented on all distributions I use
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False
fromRight :: Either a b -> b
fromRight (Right r) = r

rtrim :: String -> String
rtrim = reverse . dropWhile isSpace . reverse -- OUCH !

count :: Eq a => a -> [a] -> Int
count _ [] = 0
count el (x:xs) = (if el == x then 1 else 0) + count el xs
countWith :: (a -> Bool) -> [a] -> Int
countWith _ [] = 0
countWith f (x:xs) = (if f x then 1 else 0) + countWith f xs

map1 :: (a -> b) -> (a, c) -> (b, c)
map1 f (x, y) = (f x, y)
map2 :: (b -> c) -> (a, b) -> (a, c)
map2 g (x, y) = (x, g y)

mapL :: (a -> c) -> Either a b -> Either c b
mapL f (Left l) = Left $ f l
mapL _ (Right r) = Right r
mapR :: (b -> c) -> Either a b -> Either a c
mapR _ (Left l) = Left l
mapR g (Right r) = Right $ g r
