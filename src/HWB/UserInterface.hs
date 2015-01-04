{-# LANGUAGE RecordWildCards #-}

module HWB.UserInterface (
  UserInterface(..),
  withUserInterface
) where

import Control.Exception (bracket_)
import Control.Monad (liftM3, void)
import Graphics.UI.Gtk.Abstract.Container (containerAdd)
import Graphics.UI.Gtk.Abstract.Widget (widgetShowAll, onDestroy)
import Graphics.UI.Gtk.General.General (initGUI, mainGUI, mainQuit)
import Graphics.UI.Gtk.Scrolling.ScrolledWindow (ScrolledWindow, scrolledWindowNew)
import Graphics.UI.Gtk.WebKit.WebView (WebView, webViewNew)
import Graphics.UI.Gtk.Windows.Window (Window, windowNew)

data UserInterface = UserInterface
  { userInterfaceWindow         :: Window
  , userInterfaceScrolledWindow :: ScrolledWindow
  , userInterfaceWebView        :: WebView
  }

withUserInterface :: (UserInterface -> IO a) -> IO ()
withUserInterface callback = bracket_ initGUI mainGUI . void $ do
  ui@UserInterface{..} <- liftM3 UserInterface windowNew (scrolledWindowNew Nothing Nothing) webViewNew
  userInterfaceWindow `containerAdd` userInterfaceScrolledWindow
  userInterfaceScrolledWindow `containerAdd` userInterfaceWebView
  _ <- callback ui
  widgetShowAll userInterfaceWindow >> onDestroy userInterfaceWindow mainQuit
