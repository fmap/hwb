{-# LANGUAGE ScopedTypeVariables #-}

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
import Graphics.UI.Gtk.Scrolling.ScrolledWindow (ScrolledWindowClass, scrolledWindowGetVAdjustment)
import Graphics.UI.Gtk.WebKit.WebSettings (WebSettings, webSettingsEnableScripts, webSettingsEnablePrivateBrowsing)
import Graphics.UI.Gtk.WebKit.WebView (WebView, webViewNew, webViewLoadUri, resourceRequestStarting, webViewGetWebSettings, webViewSetWebSettings, titleChanged)
import Graphics.UI.Gtk.Windows.Window (Window, windowTitle)
import Network.URI (parseURI)
import System.Environment (getArgs)
import System.Glib.Attributes (ReadWriteAttr, AttrOp((:=)), set)
import System.Glib.Signals (on, after)

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

labelWindow :: Window -> Rule
labelWindow window = fmap void . flip (flip on titleChanged) $ \_ (title :: String) -> set window [ windowTitle := title ]

applyRules :: WebView -> [Rule] -> IO ()
applyRules view = sequence_ . map ($ view)

scrollW :: ScrolledWindowClass self => self -> (Double -> Double -> Double) -> IO ()
scrollW window binOp = do
  adjustment <- scrolledWindowGetVAdjustment window
  position <- adjustmentGetValue adjustment
  increment <- adjustmentGetStepIncrement adjustment
  adjustmentSetValue adjustment $ position `binOp` increment

jk :: ScrolledWindowClass self => self -> Rule
jk window = fmap void . flip (flip after keyPressEvent) $ do
  (k, m) <- liftM2 (,) eventKeyVal eventModifier
  liftIO . unless (m /= []) $ case k of
    106 -> window `scrollW` (+)
    107 -> window `scrollW` (-)
    _   -> return ()
  return False

main :: IO ()
main = bracket_ initGUI mainGUI . void $ do
  (window, view, scroll) <- liftM3 (,,) windowNew webViewNew (scrolledWindowNew Nothing Nothing)
  window `containerAdd` scroll
  applyRules view $ 
    [ containerAdd scroll
    , httpsEverywhere
    , noScript
    , privateBrowsing
    , labelWindow window
    , jk scroll
    ]
  getArgs >>= webViewLoadUri view . head
  widgetShowAll window >> onDestroy window mainQuit
