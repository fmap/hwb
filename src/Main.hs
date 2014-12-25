{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.Error.Util (hoistMaybe)
import Control.Exception (bracket_)
import Control.Monad (liftM2, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.HTTPSEverywhere.Rules (rewriteURL)
import Graphics.UI.Gtk (containerAdd, initGUI, mainGUI, mainQuit, onDestroy, widgetShowAll, windowNew, on)
import Graphics.UI.Gtk.WebKit.NetworkRequest (networkRequestGetUri, networkRequestSetUri)
import Graphics.UI.Gtk.WebKit.WebSettings (webSettingsEnableScripts)
import Graphics.UI.Gtk.WebKit.WebView (WebView, webViewNew, webViewLoadUri, resourceRequestStarting, webViewGetWebSettings, webViewSetWebSettings)
import Network.URI (parseURI)
import System.Environment (getArgs)
import System.Glib.Signals (ConnectId)
import System.Glib.Attributes (AttrOp((:=)), set)

noScript :: WebView -> IO ()
noScript view = do
  settings <- webViewGetWebSettings view
  set settings [webSettingsEnableScripts := False]
  webViewSetWebSettings view settings

httpsEverywhere :: WebView -> IO (ConnectId WebView)
httpsEverywhere = flip (flip on resourceRequestStarting) $ \_ _ requestM _ -> void . runMaybeT $ do
  request  <- hoistMaybe requestM
  httpsURI <- MaybeT (networkRequestGetUri request) >>= hoistMaybe . parseURI >>= liftIO . rewriteURL
  liftIO . networkRequestSetUri request $ show httpsURI

main :: IO ()
main = bracket_ initGUI mainGUI . void $ do
  (window, view) <- liftM2 (,) windowNew webViewNew
  window `containerAdd` view
  _ <- httpsEverywhere view
  _ <- noScript view
  getArgs >>= webViewLoadUri view . head
  widgetShowAll window >> onDestroy window mainQuit
