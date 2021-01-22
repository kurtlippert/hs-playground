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

fillRight :: Int -> [a] -> a -> [a]
fillRight size collection fillWith =
  take size $ concat [collection, repeat fillWith]

fillLeft :: Int -> [a] -> a -> [a]
fillLeft size collection fillWith =
  concat [take (size - length collection) $ cycle [fillWith], collection]

padRight :: Int -> [Char] -> [Char] -> [Char]
padRight size text padWith =
  fillRight size text padWith

--padLeft n coll val =
--  take n $ concat [repeat val, coll]

main :: IO ()
main = do
  print $ doIt [1,2]