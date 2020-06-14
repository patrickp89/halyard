module RegExUtil where

import Text.Regex.TDFA

-- Computes all matches for a single line.
computeMatches :: String -> String -> IO [(MatchOffset, MatchLength)]
computeMatches regEx line =
  case regEx of
    "" -> return []
    _  -> computeAllMatchesInLine regEx line


-- Returns a list of (offset, length) tuples for all matches in this string.
computeAllMatchesInLine :: String -> String -> IO [(MatchOffset, MatchLength)]
computeAllMatchesInLine regEx s = do
  let matches = getAllMatches (s =~ regEx) :: [(MatchOffset,MatchLength)]
  return matches
