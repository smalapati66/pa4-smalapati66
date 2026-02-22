module Main where
import qualified Data.Array as A
import qualified Data.Map.Lazy as M
import Data.Map.Lazy (Map, lookupGE)
import Data.Array (Array, (!))
import Data.List (minimumBy, isPrefixOf)
import Data.Ord (comparing)
import Data.Char (toLower, isLetter)

main :: IO ()
main = do
  let entropyPath = "entropy.txt"
      msgPath     = "ishmael.dec"
  entContents <- readFile entropyPath
  msgContents <- readFile msgPath
  let entMap  = parseEntropyFile entContents
      m       = rmNewlines msgContents
      (_entropyArr, wordArr) = segmentArrays entMap m
      ws = reconstruct m wordArr
      out = reformat ws
  writeFile "ishmael.seg" out

-- getting the entropy map from entropy.txt
wordsToPairs :: [[String]] -> [(String, Double)]
wordsToPairs =
  map (\[w,e] -> (w, read e)) . filter (\ws -> length ws == 2)

entropyToMap :: [[String]] -> Map String Double
entropyToMap =
  M.fromAscList . wordsToPairs

parseEntropyFile :: String -> Map String Double
parseEntropyFile =
  entropyToMap . map words . lines

-- this will remove newlines, but also assure only lowercase
rmNewlines :: String -> String
rmNewlines =
  map toLower . filter isLetter

-- DP/calculation part
maxWordLen :: Map String Double -> Int
maxWordLen em =
  maximum (1 : map length (M.keys em))

prefixCandidates :: Map String Double -> String -> Int -> [(String, Double)]
prefixCandidates em m i = go "" (drop i m)
  where
    go _ [] = []
    go p (c:cs) =
      let p' = p ++ [c]
          here =
            case M.lookup p' em of
              Just cost -> [(p', cost)]
              Nothing   -> []
          continue =
            case M.lookupGE p' em of
              Just (k,_) -> p' `isPrefixOf` k
              Nothing    -> False
      in here ++ if continue then go p' cs else []

bestAt 
  :: Map String Double 
  -> String 
  -> Array Int Double
  -> Int 
  -> (Double, String)
bestAt em m entArr i =
  case dpCandidates of
    [] -> (1/0, "") -- 1/0 sets the cost to infinity
    cs -> minimumBy (comparing fst) cs
  where 
    dpCandidates = [ (cost + entArr ! (i + length w), w)
                   | (w, cost) <- prefixCandidates em m i
                   ]

segmentArrays :: Map String Double -> String -> (Array Int Double, Array Int String)
segmentArrays em m = (entropyArr, wordArr)
  where
    n = length m
    entropyArr :: Array Int Double
    entropyArr = A.array (0, n) $
      (n, 0.0) :
      [ (i, fst (bestAt em m entropyArr i)) | i <- [n-1, n-2 .. 0] ]
    wordArr :: Array Int String
    wordArr = A.array (0, n-1)
      [ (i, snd (bestAt em m entropyArr i)) | i <- [0..n-1] ]

reconstruct :: String -> Array Int String -> [String]
reconstruct m wordArr = go 0
  where
    n = length m
    go i
      | i >= n    = []
      | otherwise =
          let w = wordArr ! i
          in if null w then [] else w : go (i + length w)

-- reformat into 60 char lines for output, words shouldn't be broken
reformat :: [String] -> String
reformat = unlines . go [] 0
  where
    go :: [String] -> Int -> [String] -> [String]
    go line _ [] =
      [unwords (reverse line) | not (null line)]
    go [] _ (w:ws)
      | length w > 60 = w : go [] 0 ws
      | otherwise     = go [w] (length w) ws
    go line len (w:ws)
      | length w > 60 =
          unwords (reverse line) : w : go [] 0 ws
      | len + 1 + length w <= 60 =
          go (w:line) (len + 1 + length w) ws
      | otherwise =
          unwords (reverse line) : go [w] (length w) ws







