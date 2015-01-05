module HWB.Plugin.Clipboard (
  yankCurrentURL
) where

import Control.Applicative ((<$>))
import Data.Maybe (fromMaybe)
import Control.Monad.IO.Class (liftIO)
import Graphics.UI.Gtk.General.Clipboard (Clipboard, clipboardGet, selectionClipboard, clipboardSetText)

import HWB.Core (H)
import HWB.Plugin.Navigation (getURL)

yankCurrentURL :: H ()
yankCurrentURL = fromMaybe "?" <$> getURL >>= liftIO . yankText

yankText :: String -> IO ()
yankText = (clipboardGet selectionClipboard >>=) . flip clipboardSetText
