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
  (buf, resultInfoLabel) <- createInputFieldsAndLabels vbox

  -- a button (TODO: remove later and use an event on the entry level):
  btn <- new Gtk.Button [#label := "Test expression!"]
  #packEnd vbox btn False False 0
  on btn #clicked (displayMatches buf resultInfoLabel)
  
  -- draw everything:
  #showAll win


createInputFieldsAndLabels :: Gtk.Box -> IO (Gtk.TextBuffer, Gtk.Label)
createInputFieldsAndLabels vbox = do
  -- an hbox for the top labels (reg ex-label and result infos):
  topRegexHbox <- new Gtk.Box [#orientation := Gtk.OrientationHorizontal]
  regexLabel <- new Gtk.Label [#label := "REGULAR EXPRESSION"]
  #packStart topRegexHbox regexLabel False False 0
  resultInfoLabel <- new Gtk.Label [#label := (resultLabelCaption 0)]
  #packEnd topRegexHbox resultInfoLabel False False 0
  #packStart vbox topRegexHbox False False 0

  -- the entry field for the regular expressions (with a placeholder RegEx):
  regExEntry <- new Gtk.Entry [#placeholderText := "a ([\\S]+) expression$"]
  #packStart vbox regExEntry False False 0

  -- a multi-line input field will collect sample text to match
  -- any RegEx against; first, a text buffer:
  buf <- new Gtk.TextBuffer []
  #setText buf "your test text goes here :)" (-1)

  -- ...then the actual text view:
  testStringsTextEntry <- Gtk.textViewNewWithBuffer buf

  -- this text field should have scrollbars, therefore we create a
  -- new GtkScrolledWindow with a proper scroll-policy:
  scrolledWindow <- new Gtk.ScrolledWindow []
  #setPolicy scrolledWindow Gtk.PolicyTypeAutomatic Gtk.PolicyTypeAutomatic

  -- place the TextView in this scrolled window
  -- and add the scrolled window to our vbox:
  Gtk.containerAdd scrolledWindow testStringsTextEntry
  #packStart vbox scrolledWindow True True 0
  return (buf, resultInfoLabel)


resultLabelCaption :: Int -> T.Text
resultLabelCaption i = T.pack ("(" ++ show i ++ " matches)")


displayMatches :: Gtk.TextBuffer -> Gtk.Label -> IO ()
displayMatches buffer label = do
  startIter <- #getStartIter buffer
  endIter <- #getEndIter buffer
  bufferText <- #getText buffer startIter endIter False
  lines <- return (show bufferText) -- TODO: matches the whole text! should be line by line instead!
  matchCount <- return (computeMatchCount lines "[a-z]+") -- TODO: use the text from the input field
  set label [#label := (resultLabelCaption matchCount)]
