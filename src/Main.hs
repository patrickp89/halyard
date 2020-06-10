{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as T
import qualified GI.Gtk as Gtk
import Data.GI.Base
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
  allLines <- getAllLines textBuffer
  regEx <- getRegEx regExBuffer
  allMatchCounts <- computeAllMatchCounts regEx allLines
  let totalMatchCount = sum allMatchCounts
  set label [#label := (resultLabelCaption totalMatchCount)]


getAllLines :: Gtk.TextBuffer -> IO [String]
getAllLines buffer = do
  lc <- #getLineCount buffer
  mapM (getSingleLine buffer) (take (fromIntegral lc) [0,1..])


getSingleLine :: Gtk.TextBuffer -> Int -> IO String
getSingleLine buffer i = do
  startIter <- #getIterAtLine buffer (fromIntegral i)
  endIter <- #getIterAtLine buffer (fromIntegral (i+1))
  bufferText <- #getText buffer startIter endIter False
  return (T.unpack bufferText)


getRegEx :: Gtk.EntryBuffer -> IO String
getRegEx regExBuffer = do
  regExText <- #getText regExBuffer
  return (T.unpack regExText)
