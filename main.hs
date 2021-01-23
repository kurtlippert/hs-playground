import Data.List
import qualified Data.IntMap as IntMap

doIt :: [a] -> [(a,a)]
doIt [] = []
doIt (x:[]) = []
doIt (x:xs) =
  ((<>) <$> fmap ((,) x) <*> doIt) xs

doIfNotEmpty :: [a] -> ([a] -> [(a,a)]) -> Maybe ([(a,a)])
doIfNotEmpty xs f =
  if null xs then Nothing else Just (f xs)


doubleSmallNumber x =
  if x > 100
    then x
    else x*2

foreverList thingToPrepend takeThisMany = 
  take takeThisMany $ thingToPrepend : [1..]

reverseForeverList thingToPrepend takeThisMany =
  reverse $ foreverList thingToPrepend takeThisMany

padRight :: Int -> [a] -> [a] -> [a]
padRight size collection padWith =
  take size $ concat [collection, cycle padWith]

padLeft :: Int -> [a] -> [a] -> [a]
padLeft size collection padWith =
  concat [take (size - length collection) $ cycle padWith, collection]

len :: (Num b) => [a] -> b
len = foldr (\x -> (+) 1) 0

len2 :: (Num b) => [a] -> b
len2 [] = 0
len2 (head:tail) = (len2 tail) + 1

main :: IO ()
main = do
  print $ doIt [1,2]
