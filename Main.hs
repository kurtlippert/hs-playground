module Main where

doIt :: [a] -> [(a,a)]
doIt [] = []
doIt [_] = []
doIt (x:xs) =
  ((<>) <$> fmap ((,) x) <*> doIt) xs

doIfNotEmpty :: [a] -> ([a] -> [(a,a)]) -> Maybe ([(a,a)])
doIfNotEmpty xs f =
  if null xs then Nothing else Just (f xs)

-- >>> doubleSmallNumber 8
-- 16
--
doubleSmallNumber :: (Ord a, Num a) => a -> a
doubleSmallNumber x =
  if x > 100
    then x
    else x*2

foreverList :: (Num a, Enum a) => a -> Int -> [a]
foreverList thingToPrepend takeThisMany = 
  take takeThisMany $ thingToPrepend : [1..]

reverseForeverList :: (Num a, Enum a) => a -> Int -> [a]
reverseForeverList thingToPrepend takeThisMany =
  reverse $ foreverList thingToPrepend takeThisMany

padRight :: Int -> [a] -> [a] -> [a]
padRight size collection padWith =
  take size $ concat [collection, cycle padWith]

padLeft :: Int -> [a] -> [a] -> [a]
padLeft size collection padWith =
  concat [take (size - length collection) $ cycle padWith, collection]

len :: (Num b) => [a] -> b
len = foldr (\_ -> (+) 1) 0

len2 :: (Num b) => [a] -> b
len2 [] = 0
len2 (_:xs) = (len2 xs) + 1

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = 
  [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0]

main :: IO ()
main = do
  print $ doIt [1,2]
