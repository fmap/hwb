{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Error.Util (hoistMaybe)
import Control.Exception (bracket, bracket_)
import Control.Monad (liftM2, liftM3, void, unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.HTTPSEverywhere.Rules (rewriteURL)
import Graphics.UI.Gtk (containerAdd, initGUI, mainGUI, mainQuit, onDestroy, widgetShowAll, scrolledWindowNew, windowNew, keyPressEvent, eventModifier, eventKeyVal)
import Graphics.UI.Gtk.Misc.Adjustment (adjustmentGetValue, adjustmentSetValue, adjustmentGetStepIncrement)
import Graphics.UI.Gtk.WebKit.NetworkRequest (networkRequestGetUri, networkRequestSetUri)
import Graphics.UI.Gtk.Scrolling.ScrolledWindow (ScrolledWindowClass, ScrolledWindow, scrolledWindowGetVAdjustment)
import Graphics.UI.Gtk.WebKit.WebSettings (WebSettings, webSettingsEnableScripts, webSettingsEnablePrivateBrowsing)
import Graphics.UI.Gtk.WebKit.WebView (WebView, webViewNew, webViewLoadUri, resourceRequestStarting, webViewGetWebSettings, webViewSetWebSettings, titleChanged)
import Graphics.UI.Gtk.Windows.Window (Window, windowTitle)
import Network.URI (parseURI)
import System.Environment (getArgs)
import System.Glib.Attributes (ReadWriteAttr, AttrOp((:=)), set)
import System.Glib.Signals (on, after)

applySettingG :: a -> WebView -> ReadWriteAttr WebSettings a a -> IO ()
applySettingG assignment view = bracket (webViewGetWebSettings view) (webViewSetWebSettings view) . flip set . return . (:= assignment)

setView, unsetView :: ReadWriteAttr WebSettings Bool Bool -> WebView -> IO ()
setView = flip $ applySettingG True; unsetView = flip $ applySettingG False

noScript, privateBrowsing :: UI -> IO ()
noScript = unsetView webSettingsEnableScripts . uiWebView
privateBrowsing = setView webSettingsEnablePrivateBrowsing . uiWebView

httpsEverywhere :: UI -> IO ()
httpsEverywhere UI{..} = void . on uiWebView resourceRequestStarting $ \_ _ requestM _ -> void . runMaybeT $ do
  request  <- hoistMaybe requestM
  httpsURI <- MaybeT (networkRequestGetUri request) >>= hoistMaybe . parseURI >>= liftIO . rewriteURL
  liftIO . networkRequestSetUri request $ show httpsURI

labelWindow :: UI -> IO ()
labelWindow UI{..} = void . on uiWebView titleChanged $ \_ (title :: String) -> do
  set uiWindow [ windowTitle := title ]

scrollW :: ScrolledWindowClass self => self -> (Double -> Double -> Double) -> IO ()
scrollW window binOp = do
  adjustment <- scrolledWindowGetVAdjustment window
  position <- adjustmentGetValue adjustment
  increment <- adjustmentGetStepIncrement adjustment
  adjustmentSetValue adjustment $ position `binOp` increment

jk :: UI -> IO ()
jk UI{..} = void . after uiWebView keyPressEvent $ do
  (k, m) <- liftM2 (,) eventKeyVal eventModifier
  liftIO . unless (m /= []) $ case k of
    106 -> uiScrolledWindow `scrollW` (+)
    107 -> uiScrolledWindow `scrollW` (-)
    _   -> return ()
  return False

data UI = UI
  { uiWindow         :: Window
  , uiScrolledWindow :: ScrolledWindow
  , uiWebView        :: WebView
  }

withUI :: (UI -> IO a) -> IO ()
withUI callback = bracket_ initGUI mainGUI . void $ do
  ui@UI{..} <- liftM3 UI windowNew (scrolledWindowNew Nothing Nothing) webViewNew
  uiWindow `containerAdd` uiScrolledWindow
  uiScrolledWindow `containerAdd` uiWebView
  _ <- callback ui
  widgetShowAll uiWindow >> onDestroy uiWindow mainQuit

main :: IO ()
main = withUI $ \ui@UI{..} -> do
  sequence_ . map ($ ui) $ 
    [ httpsEverywhere
    , noScript
    , privateBrowsing
    , labelWindow
    , jk
    ]
  getArgs >>= webViewLoadUri uiWebView . head
