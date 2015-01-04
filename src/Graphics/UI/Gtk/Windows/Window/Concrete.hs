module Graphics.UI.Gtk.Windows.Window.Concrete (
  windowTitle
) where

import System.Glib.Attributes (Attr)
import Graphics.UI.Gtk.Windows.Window (Window)
import qualified Graphics.UI.Gtk.Windows.Window as Attribute (windowTitle)

windowTitle :: Attr Window String
windowTitle = Attribute.windowTitle
