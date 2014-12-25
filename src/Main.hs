module Main (main) where

import Control.Exception (bracket_)
import Control.Monad (liftM2, void)
import Graphics.UI.Gtk (containerAdd, initGUI, mainGUI, mainQuit, onDestroy, widgetShowAll, windowNew)
import Graphics.UI.Gtk.WebKit.WebView (webViewNew, webViewLoadUri)
import System.Environment (getArgs)

main :: IO ()
main = bracket_ initGUI mainGUI . void $ do
  (window, view) <- liftM2 (,) windowNew webViewNew
  window `containerAdd` view
  getArgs >>= webViewLoadUri view . head
  widgetShowAll window >> onDestroy window mainQuit
