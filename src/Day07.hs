module Day07 where
import Text.Regex.PCRE
import GHC.SysTools.Tasks (isContainedIn)

supportsTLS :: String -> Bool
supportsTLS s = 
    let insideOK = not $ any isABBA $ getInside s
        outsideOK = any isABBA $ getOutside s
    in insideOK && outsideOK

-- | solve puzzle 2
-- >>> supportsSSL "aba[bab]xyz"
-- True
-- >>> supportsSSL "xyx[xyx]xyx"
-- False
-- >>> supportsSSL "jdpqymeesiieeeb[iwrygkpzdjttxuz]qqrbobabyedbigesuh[tmfkwpopdgcfuydhukb]mzldoxgjdeckpdvu[obojbnmmlhdwsman]nssaclvkjwmoozuissi"
-- True
-- >>> supportsSSL "aaa[kek]eke"
-- True
-- >>> supportsSSL "zazbz[bzb]cdb"
-- True
supportsSSL :: String -> Bool
supportsSSL s =
    let supernet = concatMap (getABA []) $ getOutside s
        hypernet = getInside s
        matches = [toBAB y `isContainedIn` x | x <- hypernet, y <- supernet]
    in or matches

getABA :: [String] -> String -> [String]
getABA acc (a:b:c:xs) = if a == c && a /= b then getABA ([a,b,c]:acc) (b:c:xs) else getABA acc (b:c:xs)
getABA acc _ = acc

toBAB :: String -> String
toBAB [a,b,c] = if a == c then [b, a, b] else error "a/=c: not valid ABA"
toBAB _ = error "not [a,b,c]: not valid ABA"

-- | isABBA
-- >>> isABBA "sldkjabbadslfkj"
-- True
isABBA :: String -> Bool
isABBA s = _isABBA (s =~ "(\\w)(\\w)\\2\\1" :: [[String]])
    where _isABBA :: [[String]] -> Bool
          _isABBA [] = False
          _isABBA ([_,a,b]:xs) = (a /= b) || _isABBA xs
          _isABBA _ = error "illegal regex output."

-- | getOutside 
-- >>> getOutside "abba[mnop]qrst"
-- ["abba","qrst"]
-- >>> getOutside "abba[mnop]qrst[sldk]sdfkj"
-- ["abba","qrst","sdfkj"]
getOutside :: String -> [String]
getOutside s = let m = s =~ "(?<!\\[)[a-zA-Z]+(?![^\\[\\]]*\\])" :: [[String]]
               in concat m

-- | getInside
-- >>> getInside "abba[mnop]qrst[sldk]sdfkj"
-- ["mnop","sldk"]
getInside :: String -> [String]
getInside s = let x = s =~ "\\[([a-z]+)\\]" :: [[String]]
              in map (!!1) x
