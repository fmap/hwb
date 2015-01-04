module Graphics.UI.Gtk.WebKit.WebView.Concrete (
  titleChanged,
  resourceRequestStarting
) where

import Graphics.UI.Gtk.WebKit.NetworkRequest (NetworkRequest)
import Graphics.UI.Gtk.WebKit.NetworkResponse (NetworkResponse)
import Graphics.UI.Gtk.WebKit.WebFrame (WebFrame)
import Graphics.UI.Gtk.WebKit.WebResource (WebResource)
import Graphics.UI.Gtk.WebKit.WebView (WebView)
import qualified Graphics.UI.Gtk.WebKit.WebView as Signal (titleChanged, resourceRequestStarting)
import System.Glib.Signals (Signal)

titleChanged :: Signal WebView (WebFrame -> String -> IO ())
titleChanged = Signal.titleChanged

resourceRequestStarting :: Signal WebView (WebFrame -> WebResource -> Maybe NetworkRequest -> Maybe NetworkResponse -> IO ())
resourceRequestStarting = Signal.resourceRequestStarting
