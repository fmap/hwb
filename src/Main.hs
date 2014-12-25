module Main (main) where

import Control.Error.Util (hoistMaybe)
import Control.Exception (bracket, bracket_)
import Control.Monad (liftM2, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.HTTPSEverywhere.Rules (rewriteURL)
import Graphics.UI.Gtk (containerAdd, initGUI, mainGUI, mainQuit, onDestroy, widgetShowAll, windowNew, on)
import Graphics.UI.Gtk.WebKit.NetworkRequest (networkRequestGetUri, networkRequestSetUri)
import Graphics.UI.Gtk.WebKit.WebSettings (WebSettings, webSettingsEnableScripts, webSettingsEnablePrivateBrowsing)
import Graphics.UI.Gtk.WebKit.WebView (WebView, webViewNew, webViewLoadUri, resourceRequestStarting, webViewGetWebSettings, webViewSetWebSettings)
import Network.URI (parseURI)
import System.Environment (getArgs)
import System.Glib.Attributes (ReadWriteAttr, AttrOp((:=)), set)

type Rule = WebView -> IO ()

applySettingG :: a -> WebView -> ReadWriteAttr WebSettings a a -> IO ()
applySettingG assignment view = bracket (webViewGetWebSettings view) (webViewSetWebSettings view) . flip set . return . (:= assignment)

setView, unsetView :: ReadWriteAttr WebSettings Bool Bool -> Rule
setView = flip $ applySettingG True; unsetView = flip $ applySettingG False

noScript, privateBrowsing :: Rule
noScript = unsetView webSettingsEnableScripts
privateBrowsing = setView webSettingsEnablePrivateBrowsing

httpsEverywhere :: Rule
httpsEverywhere = fmap void . flip (flip on resourceRequestStarting) $ \_ _ requestM _ -> void . runMaybeT $ do
  request  <- hoistMaybe requestM
  httpsURI <- MaybeT (networkRequestGetUri request) >>= hoistMaybe . parseURI >>= liftIO . rewriteURL
  liftIO . networkRequestSetUri request $ show httpsURI

applyRules :: WebView -> [Rule] -> IO ()
applyRules view = sequence_ . map ($ view)

main :: IO ()
main = bracket_ initGUI mainGUI . void $ do
  (window, view) <- liftM2 (,) windowNew webViewNew
  applyRules view $ 
    [ containerAdd window
    , httpsEverywhere
    , noScript
    , privateBrowsing
    ]
  getArgs >>= webViewLoadUri view . head
  widgetShowAll window >> onDestroy window mainQuit
