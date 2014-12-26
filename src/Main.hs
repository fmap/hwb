{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE LambdaCase          #-}

module Main (main) where

import Prelude hiding (Either(..))
import Control.Concurrent.STM.TMVar (TMVar, newTMVar, takeTMVar, putTMVar, readTMVar)
import Control.Error.Util (hoistMaybe)
import Control.Exception (bracket, bracket_)
import Control.Monad (liftM2, liftM4, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM (atomically)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.HTTPSEverywhere.Rules (rewriteURL)
import Graphics.UI.Gtk (containerAdd, initGUI, mainGUI, mainQuit, onDestroy, widgetShowAll, scrolledWindowNew, windowNew, keyPressEvent, eventModifier, eventKeyVal)
import Graphics.UI.Gtk.Gdk.Keys (KeyVal)
import Graphics.UI.Gtk.Gdk.EventM (Modifier)
import Graphics.UI.Gtk.Misc.Adjustment (adjustmentGetValue, adjustmentSetValue, adjustmentGetUpper, adjustmentGetLower)
import Graphics.UI.Gtk.WebKit.NetworkRequest (networkRequestGetUri, networkRequestSetUri)
import Graphics.UI.Gtk.Scrolling.ScrolledWindow (ScrolledWindow, scrolledWindowGetVAdjustment,scrolledWindowGetHAdjustment)
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

(&) :: a -> (a -> b) -> b
(&) = flip ($)
infixr 0 &

data Orientation = Horizontal | Vertical

data Position a  = Absolute a | Relative a

scroll :: ScrolledWindow -> Orientation -> Position Double -> IO ()
scroll window orientation position = do
  adjustment <- window & case orientation of 
    Horizontal -> scrolledWindowGetHAdjustment
    Vertical   -> scrolledWindowGetVAdjustment
  adjustmentSetValue adjustment =<< case position of
    Absolute n -> return n
    Relative n -> fmap (+n) $ adjustmentGetValue adjustment

scrollBottom :: ScrolledWindow -> IO ()
scrollBottom window = scrolledWindowGetVAdjustment window
                  >>= adjustmentGetUpper
                  >>= scroll window Vertical . Absolute

scrollTop :: ScrolledWindow -> IO ()
scrollTop window = scrolledWindowGetVAdjustment window
               >>= adjustmentGetLower
               >>= scroll window Vertical . Absolute

data Buffer a = Buffer (Maybe a) (Maybe a) deriving Show

emptyBuffer :: Buffer a
emptyBuffer = Buffer Nothing Nothing

pushBuffer :: a -> Buffer a -> Buffer a
pushBuffer c (Buffer _ b) = Buffer b $ Just c

jk :: UI -> IO ()
jk UI{..} = void . after uiWebView keyPressEvent $ do
  (k, m) <- liftM2 (,) eventKeyVal eventModifier
  liftIO . atomically $ takeTMVar uiKeyBuffer >>= putTMVar uiKeyBuffer . pushBuffer (k,m)
  liftIO $ atomically (readTMVar uiKeyBuffer) >>= \case
    Buffer (Just (103, [])) (Just (103, [])) -> scrollTop uiScrolledWindow
    Buffer _ (Just (106, [])) -> scroll uiScrolledWindow Vertical $ Relative 200
    Buffer _ (Just (107, [])) -> scroll uiScrolledWindow Vertical $ Relative (-200)
    Buffer _ (Just (71, _)) -> scrollBottom uiScrolledWindow
    _   -> return ()
  return False

data UI = UI
  { uiWindow         :: Window
  , uiScrolledWindow :: ScrolledWindow
  , uiWebView        :: WebView
  , uiKeyBuffer      :: TMVar (Buffer (KeyVal, [Modifier]))
  }

withUI :: (UI -> IO a) -> IO ()
withUI callback = bracket_ initGUI mainGUI . void $ do
  ui@UI{..} <- liftM4 UI windowNew (scrolledWindowNew Nothing Nothing) webViewNew (atomically $ newTMVar emptyBuffer)
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
