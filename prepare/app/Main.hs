module Main where

import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Data.List.Split (splitOn)
import Data.Char (isAscii, isLetter, toLower)

main :: IO ()
main = do
  let inputPath = "enwiki-2023-04-13.txt"
      outputPath = "entropy.txt"
  contents <- readFile inputPath
  let fs    = [ ws | line <- lines contents, let ws = words line]
      fs'   = hyphenSplit (asciiFilter fs)
      entMap = entropyMap fs'
      outStr = toOutput entMap
  writeFile outputPath outStr

-- first, turning input into map of frequencies
asciiFilter :: [[String]] -> [[String]]
asciiFilter =
  filter (\ws -> all isAscii (head ws))

onlyLetters :: String -> String
onlyLetters =
  map toLower . filter isLetter

hyphenSplit :: [[String]] -> [[String]]
hyphenSplit = 
  concatMap (\[w,n] -> [[subw, n] | s <- splitOn "-" w
                                  , let subw = onlyLetters s
                                  , not (null subw)])

mFromFreq :: [[String]] -> Map String Int
mFromFreq = 
  M.filter (>= 50) . M.unionsWith (+) . map (\[w,n] -> M.singleton w (read n))

-- Entropy calculation stuff
log2 :: Floating a => a -> a
log2 x = log x / log 2

calcEntropy :: Int -> Int -> Double
calcEntropy total n =
  let p = fromIntegral n / fromIntegral total   -- this is p(w)
  in - log2 p

entropyMap :: [[String]] -> Map String Double
entropyMap fs =
  let m     = mFromFreq fs
      total = sum (M.elems m)
  in M.map (calcEntropy total) m

-- Final output string from entropy map
toOutput :: Map String Double -> String
toOutput m = 
  unlines [w ++ " " ++ show e | (w,e) <- M.toAscList m]


