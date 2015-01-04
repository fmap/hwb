module Graphics.UI.Gtk.Abstract.Widget.Concrete (
  keyPressEvent
) where

import qualified Graphics.UI.Gtk.Abstract.Widget as Signal (keyPressEvent)
import Graphics.UI.Gtk.Gdk.EventM (EventM, EKey)
import Graphics.UI.Gtk.WebKit.WebView (WebView)
import System.Glib.Signals (Signal)

keyPressEvent :: Signal WebView (EventM EKey Bool)
keyPressEvent = Signal.keyPressEvent

