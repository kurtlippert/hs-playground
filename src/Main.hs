{-# LANGUAGE TupleSections #-}

module Main where

-- import Data.List
-- import qualified Data.IntMap as IntMap

doIt :: [a] -> [(a,a)]
doIt [] = []
doIt [x] = []
doIt (x:xs) =
  ((<>) <$> fmap (x,) <*> doIt) xs

-- doIfNotEmpty :: [a] -> ([a] -> [(a,a)]) -> Maybe ([(a,a)])
-- doIfNotEmpty xs f =
--   if null xs then Nothing else Just (f xs)

-- doubleSmallNumber x =
--   if x > 100
--     then x
--     else x*2

foreverList :: (Num a, Enum a) => a -> Int -> [a]
foreverList thingToPrepend takeThisMany = 
  take takeThisMany $ thingToPrepend : [1..]

reverseForeverList :: (Num a, Enum a) => a -> Int -> [a]
reverseForeverList thingToPrepend takeThisMany =
  reverse $ foreverList thingToPrepend takeThisMany

padRight :: Int -> [a] -> [a] -> [a]
padRight size collection padWith =
  take size (collection ++ cycle padWith)

padLeft :: Int -> [a] -> [a] -> [a]
padLeft size collection padWith =
  take (size - length collection) $ cycle padWith ++ collection

len :: (Num b) => [a] -> b
len = foldr (\x -> (+) 1) 0

{-| New way to calculate length
>>> len2 []
0

>>> len2 [1]
1

>>> len2 [1,2,3]
3
-}
len2 :: (Num b) => [a] -> b
len2 [] = 0
len2 (head:tail) = len2 tail + 1

{-|
>>> calcBmis [(23.1, 33.2)]
[]
-}
calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = 
  [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0]

main :: IO ()
main = do
  print $ doIt [1,2]
