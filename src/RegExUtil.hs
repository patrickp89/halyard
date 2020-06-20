module RegExUtil where

import Text.Regex.TDFA
import Text.Regex.TDFA.ReadRegex

-- Checks if Text.Regex.TDFA can parse the given RegEx.
checkRegEx :: String -> IO Bool
checkRegEx regEx =
  case parseRegex regEx of
    Left e -> do
      print e
      return False
    Right _ -> return True


-- Computes all matches for a single line. Returns a list of (offset, length)
-- tuples for all matches in this string.
computeMatches :: String -> String -> IO [(MatchOffset, MatchLength)]
computeMatches regEx line = do
  let matches = getAllMatches (line =~ regEx) :: [(MatchOffset,MatchLength)]
  return matches
