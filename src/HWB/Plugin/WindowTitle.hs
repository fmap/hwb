module HWB.Plugin.WindowTitle (
  setWindowTitle
) where

import Control.Applicative ((<$>),(<*>))
import Control.Monad (void)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.Reader (ask)
import Control.Monad.Trans.State (get)
import Graphics.UI.Gtk.WebKit.WebFrame (WebFrame)
import Graphics.UI.Gtk.Windows.Window.Concrete (windowTitle)

import HWB.Core (H, runH, is)

setWindowTitle :: H (WebFrame -> String -> IO ())
setWindowTitle = runH <$> lift get <*> ask >>= \unH ->
  return $ \_ title -> void . unH $ windowTitle `is` title
