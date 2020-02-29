{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified GI.Gtk as Gtk
import Data.GI.Base

main :: IO ()
main =
  do
    Gtk.init Nothing
    win <- new Gtk.Window [#title := "halyard - RegEx Tester"]
    on win #destroy Gtk.mainQuit
    #resize win 640 480
    box <- new Gtk.Box [#orientation := Gtk.OrientationVertical]
    #add win box

    -- the entry field for the regular expressions (with a default RegEx):
    regExEntry <- new Gtk.Entry [#placeholderText := "a ([\\S]+) expression$"]
    #packStart box regExEntry False False 0

    -- create a TextView, get the its buffer and set a default text:
    testTextEntry <- new Gtk.TextView [#wrapMode := Gtk.WrapModeWordChar]
    buf <- testTextEntry `get` #buffer
    --  TODO: buf `set_text` "Test eins zwei drei"
    #packStart box testTextEntry False False 0

    -- a button (TODO: remove later and use an event on the entry level):
    btn <- new Gtk.Button [#label := "Test expression"]
    #packStart box btn False False 0
    
    #showAll win
    Gtk.main
