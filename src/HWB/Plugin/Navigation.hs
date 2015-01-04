module HWB.Plugin.Navigation (
  setURL
) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (asks)
import Graphics.UI.Gtk.WebKit.WebView (webViewLoadUri)

import HWB.Core (H)
import HWB.UserInterface (UserInterface(userInterfaceWebView))

setURL :: String -> H ()
setURL url = asks userInterfaceWebView >>= liftIO . flip webViewLoadUri url
