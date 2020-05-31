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
    box <- new Gtk.Box [#orientation := Gtk.OrientationVertical]
    #add win box

    -- the entry field for the regular expressions (with a default RegEx):
    regExEntry <- new Gtk.Entry [#placeholderText := "a ([\\S]+) expression$"]
    #packStart box regExEntry False False 0


    -- next, we need a multi-line input field that will collect text to
    -- match the provided RegEx against; first, a text buffer:
    buf <- new Gtk.TextBuffer []
    #setText buf "your test text goes here :)" (-1)

    -- ...then the actual text view:
    testStringsTextEntry <- Gtk.textViewNewWithBuffer buf

    -- TODO: scroll bars etc.!
    #packStart box testStringsTextEntry False False 0

    -- a button (TODO: remove later and use an event on the entry level):
    btn <- new Gtk.Button [#label := "Test expression!"]
    #packStart box btn False False 0
    -- on btn #clicked (set msg [#label := "Clickeddd"])
    
    #showAll win
    Gtk.main
