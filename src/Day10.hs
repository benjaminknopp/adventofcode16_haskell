-- | Day 10: Balance Bots
-- 1. take botMap (sorted by number of values) and pop bot with two entries
-- 2. search for receivers in instructions
-- 3. update receiver-bots in botlist
-- 4. remove giver-bot
-- repeat until giving bot values are Set [17, 61]
module Day10 where
import Text.Regex.PCRE
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as Set

type Bot = Int
type Value = Int
-- receivers of low and high value
type Low = Int
type High = Int

data Receiver = Output Value | Bot Value deriving (Show)
instance Read Receiver where
    readsPrec _ s = case words s of
                        ["bot", i] -> [(Bot (read i), "")]
                        ["output", i] -> [(Output (read i), "")]
                        _ -> error "parsing Receiver failed"

main :: IO ()
main = interact $ show . solve . lines

example :: [String]
example = ["value 5 goes to bot 2",
           "bot 2 gives low to bot 1 and high to bot 0",
           "value 3 goes to bot 1",
           "bot 1 gives low to output 1 and high to bot 0",
           "bot 0 gives low to output 2 and high to output 0",
           "value 2 goes to bot 2"]

solve :: [String]  -> ([(Bot, Set Value)], Value)
solve lstOLines = let instructions = getInstructions lstOLines
                      botMap =  getBotMap lstOLines
                      (botMap', botList) = updateBotList instructions botMap []
                      outputs = M.elems $ M.filterWithKey (\k _ -> k > -4) botMap'
                  in (filter ((==Set.fromList [17, 61]).snd) botList, 
                      product $ concatMap Set.toList outputs)

updateBotList :: M.Map Bot (Receiver, Receiver) 
        -> M.Map Bot (Set Value) 
        -> [(Bot, Set Value)]
        -> (M.Map Bot (Set Value), [(Bot, Set Value)])
updateBotList instructions botMap botList
    | botMap == M.empty = (M.empty, botList)
    | all (<0) (M.keys botMap) = (botMap, botList)
    | null (M.toList $ M.filter ((>1).length)  botMap) = (botMap, botList)
    | otherwise = let 
                    (botMap', bot) = popBot botMap
                    (lowReceiver, highReceiver) = case instructions M.! fst bot of
                        (Bot a, Bot b) -> (a, b)
                        (Output a, Bot b) -> (-a-1, b)
                        (Bot a, Output b) -> (a, -b-1)
                        (Output a, Output b) -> (-a-1, -b-1)
                    (low, high) = Set.splitAt 1 (snd bot)
                    botMap'' = M.insertWith Set.union lowReceiver low botMap'
                    botMap''' = M.insertWith Set.union highReceiver high botMap''
                    newBotList = bot:botList
                  in updateBotList instructions botMap''' newBotList

getInstructions :: [String] -> M.Map Bot (Receiver, Receiver)
getInstructions = M.fromList . map (g.f) . filter ((=="bot").take 3)
    where -- f = (=~ "[(\\d)]+") :: String -> [[String]]
          f = (=~ "output [\\d]+|bot [\\d]+") :: String -> [[String]]
          -- g [[a],[b],[c]] = (read a, (read b, read c)) :: (Int, (Int, Int))
          g [[a], [b], [c]] = (read $ (last.words) a, (read b, read c)) :: (Int, (Receiver, Receiver))
          g _ =  error "parse error"

-- | take botMap and pop bot with two entries
-- >>> popBot (M.fromList [(1,Set.fromList [3]),(2,Set.fromList [2,5])]) 
-- (M.fromList [(1,Set.fromList [3])], (2,Set.fromList [2,5]))
popBot :: M.Map Bot (Set Value) -> (M.Map Bot (Set Value), (Bot, Set Value))
popBot m = 
    let omap = M.toList $ M.filter ((>1).length)  m
        senderBot = head omap
        m' = M.delete (fst senderBot) m
    in (m', senderBot)

getBotMap :: [String] -> M.Map Bot (Set Value)
getBotMap = M.fromListWith Set.union . map valueBot . filter ((=="value").take 5)

valueBot :: String -> (Bot, Set Value)
valueBot s = toTuple $ s =~ "[(\\d)]+"

toTuple :: [[String]] -> (Bot, Set Value)
toTuple [[a], [b]] = (read b, Set.singleton (read a :: Int))
toTuple _ = error "parse error"
