module HWB.Plugin.Navigation (
  setURL,
  getURL,
  goBack,
  goForward,
  reload,
  reloadBypassCache
) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (asks)
import Graphics.UI.Gtk.WebKit.WebView (webViewLoadUri, webViewGetUri, webViewGoBack, webViewGoForward, webViewReload, webViewReloadBypassCache)

import HWB.Core (H)
import HWB.UserInterface (UserInterface(userInterfaceWebView))

setURL :: String -> H ()
setURL url = asks userInterfaceWebView >>= liftIO . flip webViewLoadUri url

getURL :: H (Maybe String)
getURL = asks userInterfaceWebView >>= liftIO . webViewGetUri

goBack :: H ()
goBack = asks userInterfaceWebView >>= liftIO . webViewGoBack

goForward :: H ()
goForward = asks userInterfaceWebView >>= liftIO . webViewGoForward

reload :: H ()
reload = asks userInterfaceWebView >>= liftIO . webViewReload

reloadBypassCache :: H()
reloadBypassCache = asks userInterfaceWebView >>= liftIO . webViewReloadBypassCache
