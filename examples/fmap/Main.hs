module Main (
  main
) where

import Control.Monad.IO.Class (liftIO)
import System.Environment (getArgs)

import Graphics.UI.Gtk.Abstract.Widget.Concrete (keyPressEvent) 
import Graphics.UI.Gtk.WebKit.WebSettings.Concrete (webSettingsEnableScripts, webSettingsEnablePrivateBrowsing)
import Graphics.UI.Gtk.WebKit.WebView.Concrete (titleChanged, resourceRequestStarting)

import HWB.Core (calls, is)
import HWB.Main (hwb)
import HWB.Plugin.Clipboard (yankCurrentURL)
import HWB.Plugin.HTTPSEverywhere (tryHTTPS)
import HWB.Plugin.Keybinding (Keybinding(..), tryKeybindings)
import HWB.Plugin.Navigation (setURL)
import HWB.Plugin.Scrolling (scrollTop, scrollBottom, pageDown, pageUp)
import HWB.Plugin.WindowTitle (setWindowTitle)

keys :: [Keybinding]
keys =
  [ "gg" :== scrollTop
  , "j"  :== pageDown
  , "k"  :== pageUp
  , "G"  :== scrollBottom
  , "yy" :== yankCurrentURL
  ]

main :: IO ()
main = hwb $ do
  webSettingsEnableScripts `is` False
  webSettingsEnablePrivateBrowsing `is` True
  resourceRequestStarting `calls` tryHTTPS
  titleChanged `calls` setWindowTitle
  keyPressEvent `calls` tryKeybindings keys
  liftIO getArgs >>= setURL . head
