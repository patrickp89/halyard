{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified GI.Gtk as Gtk
import Data.GI.Base

main :: IO ()
main =
  do
    Gtk.init Nothing

    -- create the window:
    win <- new Gtk.Window [#title := "halyard - RegEx Tester"]
    on win #destroy Gtk.mainQuit
    #resize win 640 480

    -- and a vbox:
    vbox <- new Gtk.Box [#orientation := Gtk.OrientationVertical]
    #add win vbox


    -- an hbox for the top labels (reg ex-label and result infos):
    topRegexHbox <- new Gtk.Box [#orientation := Gtk.OrientationHorizontal]
    regexLabel <- new Gtk.Label [#label := "REGULAR EXPRESSION"]
    #packStart topRegexHbox regexLabel False False 0
    resultInfoLabel <- new Gtk.Label [#label := "(0 matches)"]
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


    -- a button (TODO: remove later and use an event on the entry level):
    btn <- new Gtk.Button [#label := "Test expression!"]
    #packEnd vbox btn False False 0
    -- on btn #clicked (set msg [#label := "Clickeddd"])
    
    -- draw everything:
    #showAll win
    Gtk.main
