module RegExUtil where

import Text.Regex.TDFA

computeAllMatchCounts :: String -> [String] -> IO [Int]
computeAllMatchCounts regEx lines =
  case regEx of
    "" -> return []
    _  -> mapM (computeMatchCount regEx) lines


computeMatchCount :: String -> String -> IO Int
computeMatchCount regEx s = do
  let matches = (getAllTextMatches (s =~ regEx) :: [String])
  return (length matches)
