module Day06 where
import GHC.Utils.Misc ( chunkList )
import qualified Data.Map as Map
import Data.Ord
import Data.Sort (sortOn)

main :: IO ()
main = do
    x <- getContents
    let result = map (fst . head . count) $ toVert $ lines x
    let result2 = map (fst . last . count) $ toVert $ lines x
    print result
    print result2

-- | transpose list of String
-- >>> toVert ["ab","ac","bc"]
-- ["aab","bcc"]
toVert :: [[Char]] -> [[Char]]
toVert lol = 
    let m = length lol
        n = length $ head lol
    in chunkList m [lol!!j!!i |i<-[0..(n-1)],j<-[0..(m-1)]]

-- | count letters
-- >>> count "ababa"
-- Map.fromList [('a',3),('b',2)]
-- >>> count "notarealroom"
-- Map.fromList [('o',3),('a',2),('r',2),('e',1),('l',1),('m',1),('n',1),('t',1)]
-- count :: String -> Map.Map Char Int
count :: String -> [(Char, Int)]
count = sortOn (Data.Ord.Down . snd) . Map.assocs . foldl _feed Map.empty

_feed :: Map.Map Char Int -> Char -> Map.Map Char Int
_feed acc c = Map.insertWith (+) c 1 acc
