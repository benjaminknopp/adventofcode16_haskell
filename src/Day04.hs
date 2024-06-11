module Day04 where
import Text.Regex.Posix
import qualified Data.Map as Map
import Data.Sort (sortOn)
import Data.Ord
import Data.Char ( ord, chr )
import Data.List ( isInfixOf )


main :: IO ()
main = do
    content <- getContents
    let y = map (decipher . splitDash) $ lines content
        result = sumIds y
        result2 = filter searchNorth y
    print result
    print result2

searchNorth :: Maybe (String, Int) -> Bool
searchNorth Nothing = False
searchNorth (Just (code, _)) =  "north" `isInfixOf` code

sumIds :: [Maybe (String, Int)] -> Int
sumIds = foldl _sum 0

_sum :: Int -> Maybe (String, Int) -> Int
_sum i Nothing = i
_sum i (Just (_, v)) = i + v

decipher :: [String] -> Maybe (String, Int)
decipher xs = 
    let y = init xs
        (i, code) = (parseLast . last) xs
        w = (encode . concat . init) xs
        isData = code == w
        label = unwords $ map (decipherWord i) y
    in if isData then Just (label, i) else Nothing

decipherWord :: Int -> String -> String
decipherWord i = map (decipherChar i)

decipherChar :: Int -> Char -> Char
decipherChar i c = chr $ (ord c - 97 + i) `mod` 26 + 97

encode :: String -> String
encode = take 5 . map fst . count

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

-- | split input
-- >>> splitDash "xjinphzm-bmvyz-hvbizodx-xviyt-xjvodib-ozxcijgjbt-343[ixbjv]"
-- ["xjinphzm","bmvyz","hvbizodx","xviyt","xjvodib","ozxcijgjbt","343[ixbjv]"]
splitDash :: String -> [String]
splitDash = words . reverse . foldl replace ""

-- |
-- ([1-9]*)\[([a-z]+)]
-- >>> parseLast "723[rwmzt]" 
-- (723,"rwmzt")
parseLast :: String -> (Int, String)
parseLast s = case s =~ "([0-9]*)\\[([a-z]+)\\]" of
    [[_, b, c]] -> (read b, c)
    _ -> error "can not parse last part of input."

replace :: String -> Char -> String
replace s c = if c == '-' then ' ':s else c:s
-- xjinphzm-bmvyz-hvbizodx-xviyt-xjvodib-ozxcijgjbt-343[ixbjv]
-- ([a-z]+)-([a-z]+)-([a-z]+)-(\d+)-([a-z]+)-([a-z]+)-(\d+)\[([a-z]+)\]
