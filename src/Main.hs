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
import HWB.Plugin.HTTPSEverywhere (tryHTTPS)
import HWB.Plugin.Keybinding (Keybinding(..), tryKeybindings)
import HWB.Plugin.Navigation (setURL)
import HWB.Plugin.Scrolling (Orientation(..), Position(..), scroll, scrollTop, scrollBottom)
import HWB.Plugin.WindowTitle (setWindowTitle)

keys :: [Keybinding]
keys =
  [ "gg" :== scrollTop
  , "j"  :== scroll Vertical $ Relative 200
  , "k"  :== scroll Vertical $ Relative (-200)
  , "G"  :== scrollBottom
  ]

main :: IO ()
main = hwb $ do
  webSettingsEnableScripts `is` False
  webSettingsEnablePrivateBrowsing `is` True
  resourceRequestStarting `calls` tryHTTPS
  titleChanged `calls` setWindowTitle
  keyPressEvent `calls` tryKeybindings keys
  liftIO getArgs >>= setURL . head
