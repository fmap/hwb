module HWB.Plugin.Navigation (
  setURL,
  getURL
) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (asks)
import Graphics.UI.Gtk.WebKit.WebView (webViewLoadUri, webViewGetUri)

import HWB.Core (H)
import HWB.UserInterface (UserInterface(userInterfaceWebView))

setURL :: String -> H ()
setURL url = asks userInterfaceWebView >>= liftIO . flip webViewLoadUri url

getURL :: H (Maybe String)
getURL = asks userInterfaceWebView >>= liftIO . webViewGetUri
