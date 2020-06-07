module RegExUtil where

import Text.Regex.TDFA

computeMatchCount :: String -> String -> Int
computeMatchCount s regEx =
  let matches = getAllTextMatches (s =~ regEx) :: [String] in
  length matches
