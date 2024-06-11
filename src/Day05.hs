module Day05 where
import Crypto.Hash ( hashlazy, MD5, Digest )
import Data.ByteString.Lazy.Internal
import qualified Data.Map as M
import Data.Maybe (catMaybes)

getPassword :: String -> String
getPassword doorID = take 8 $ foldr (filterHash doorID) "" [0..]

filterHash :: String -> Int -> String -> String
filterHash doorID i acc = let h = hash doorID i
                          in if first5zero h then h!!5:acc else acc

first5zero :: String -> Bool
first5zero s = all ('0' ==) (take 5 s)

hash :: String -> Int -> String
hash s = let digest = hashlazy . packChars . (++) s . show :: Int -> Digest MD5
         in show . digest

-- from
-- https://github.com/purcell/adventofcode2016/blob/master/src/Day5.hs
solutionB :: [String] -> String
solutionB hs = M.elems $ head $ dropWhile incomplete $ scanl addPair M.empty pairs
  where
    addPair m (p, c) =
      if p `M.member` m
        then m
        else M.insert p c m
    incomplete m = M.keys m /= "01234567"
    pairs = catMaybes $ posAndChar . drop 5 <$> hs
    posAndChar (p:c:_)
      | p >= '0' && p < '8' = Just (p, c)
    posAndChar _ = Nothing
