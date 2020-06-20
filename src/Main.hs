{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as T
import qualified GI.Gtk as Gtk
import Data.GI.Base
import Text.Regex.TDFA
import RegExUtil

main :: IO ()
main = do
  Gtk.init Nothing
  createUiWidgets
  Gtk.main


createUiWidgets :: IO ()
createUiWidgets = do
  -- create the window:
  win <- new Gtk.Window [#title := "halyard - RegEx Tester"]
  on win #destroy Gtk.mainQuit
  #resize win 640 480

  -- and a vbox:
  vbox <- new Gtk.Box [#orientation := Gtk.OrientationVertical]
  #add win vbox

  -- create all input fields and their corresponding labels:
  (textBuffer, regExBuffer, resultInfoLabel) <- createInputFieldsAndLabels vbox

  -- a button (TODO: remove later and use an event on the entry level):
  btn <- new Gtk.Button [#label := "Test expression!"]
  #packEnd vbox btn False False 0
  on btn #clicked (displayMatches textBuffer regExBuffer resultInfoLabel)
  
  -- draw everything:
  #showAll win


createInputFieldsAndLabels :: Gtk.Box -> IO (Gtk.TextBuffer, Gtk.EntryBuffer, Gtk.Label)
createInputFieldsAndLabels vbox = do
  -- an hbox for the top labels (reg ex-label and result infos):
  topRegexHbox <- new Gtk.Box [#orientation := Gtk.OrientationHorizontal]
  regexLabel <- new Gtk.Label [#label := "REGULAR EXPRESSION"]
  #packStart topRegexHbox regexLabel False False 0
  resultInfoLabel <- new Gtk.Label [#label := (resultLabelCaption 0)]
  #packEnd topRegexHbox resultInfoLabel False False 0
  #packStart vbox topRegexHbox False False 0

  -- the entry field for the regular expressions (with a placeholder RegEx):
  regExEntry <- new Gtk.Entry [#placeholderText := "^your [a-z]+ expression$"]
  #packStart vbox regExEntry False False 0
  regExBuffer <- #getBuffer regExEntry

  -- a multi-line input field will collect sample text to match
  -- any RegEx against; first, a text buffer:
  textBuffer <- new Gtk.TextBuffer []
  #setText textBuffer "your test text goes here :)" (-1)

  -- ...then the actual text view:
  testStringsTextEntry <- Gtk.textViewNewWithBuffer textBuffer

  -- this text field should have scrollbars, therefore we create a
  -- new GtkScrolledWindow with a proper scroll-policy:
  scrolledWindow <- new Gtk.ScrolledWindow []
  #setPolicy scrolledWindow Gtk.PolicyTypeAutomatic Gtk.PolicyTypeAutomatic

  -- place the TextView in this scrolled window
  -- and add the scrolled window to our vbox:
  Gtk.containerAdd scrolledWindow testStringsTextEntry
  #packStart vbox scrolledWindow True True 0

  -- return the buffers and the label:
  return (textBuffer, regExBuffer, resultInfoLabel)


resultLabelCaption :: Int -> T.Text
resultLabelCaption i = T.pack ("(" ++ show i ++ " matches)")


displayMatches :: Gtk.TextBuffer -> Gtk.EntryBuffer -> Gtk.Label -> IO ()
displayMatches textBuffer regExBuffer label = do
  -- un-highlight any old matches:
  removeAllMarkup textBuffer
  -- then compute all new matches and highlight them:
  allLines <- getAllLines textBuffer
  maybeRegEx <- getRegEx regExBuffer
  case maybeRegEx of
    Nothing -> set label [#label := (resultLabelCaption 0)]
    Just regEx -> do
      highlightedLines <- mapM (computeAndHighlightMatches textBuffer regEx) allLines
      let totalMatchCount = sum highlightedLines
      set label [#label := (resultLabelCaption totalMatchCount)]


removeAllMarkup :: Gtk.TextBuffer -> IO ()
removeAllMarkup textBuffer = do
  startIter <- #getStartIter textBuffer
  endIter <- #getEndIter textBuffer
  #removeAllTags textBuffer startIter endIter


-- Highlights all matches in a single line, returns the match count.
computeAndHighlightMatches :: Gtk.TextBuffer -> String -> (Int, String) -> IO Int
computeAndHighlightMatches textBuffer regEx (lineNumber, line) = do
  allMatchesInThisLine <- computeMatches regEx line
  let matchCount = length allMatchesInThisLine
  markups <- mapM (highlightSingleMatch textBuffer lineNumber) allMatchesInThisLine
  return matchCount


highlightSingleMatch :: Gtk.TextBuffer -> Int -> (MatchOffset, MatchLength) -> IO T.Text
highlightSingleMatch textBuffer lineNumber (offset, l) = do
  -- get the text that should be highlighted, first:
  startIter <- #getIterAtLineOffset textBuffer (fromIntegral lineNumber) (fromIntegral offset)
  endIter <- #getIterAtLineOffset textBuffer (fromIntegral lineNumber) (fromIntegral (offset + l))
  match <- #getText textBuffer startIter endIter False
  -- delete the old match:
  #delete textBuffer startIter endIter
  -- ...and insert the new one with the match-highlight markup:
  let markup = T.pack ("<span background=\"PaleTurquoise\" >" ++ T.unpack match ++ "</span>")
  -- TODO: to distinguish between matches that are right next to each other, the color should be alternating!
  #insertMarkup textBuffer startIter markup (-1)
  return markup


-- Returns a list of (lineNumber, line) for all lines in the buffer.
getAllLines :: Gtk.TextBuffer -> IO [(Int, String)]
getAllLines buffer = do
  lc <- #getLineCount buffer
  mapM (getSingleLine buffer) (take (fromIntegral lc) [0,1..])


-- Extracts the line at lineNumber from the buffer and returns (lineNumber, line).
getSingleLine :: Gtk.TextBuffer -> Int -> IO (Int, String)
getSingleLine textBuffer lineNumber = do
  startIter <- #getIterAtLine textBuffer (fromIntegral lineNumber)
  endIter <- #getIterAtLine textBuffer (fromIntegral (lineNumber + 1))
  bufferText <- #getText textBuffer startIter endIter False
  return (lineNumber, (T.unpack bufferText))


-- Extracts the user's regular expression and checks whether it is valid..
getRegEx :: Gtk.EntryBuffer -> IO (Maybe String)
getRegEx regExBuffer = do
  regExText <- #getText regExBuffer
  let regEx = T.unpack regExText
  isValid <- checkRegEx regEx
  if isValid then return (Just regEx) else return Nothing
