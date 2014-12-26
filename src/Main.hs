{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}

module Main (main) where

import Prelude hiding (Either(..))
import Control.Concurrent.STM.TMVar (TMVar, newTMVar, takeTMVar, putTMVar, readTMVar)
import Control.Error.Util (hoistMaybe)
import Control.Exception (bracket, bracket_)
import Control.Monad (liftM4, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM (atomically)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.HTTPSEverywhere.Rules (rewriteURL)
import Graphics.UI.Gtk (containerAdd, initGUI, mainGUI, mainQuit, onDestroy, widgetShowAll, scrolledWindowNew, windowNew, keyPressEvent, eventKeyVal)
import Graphics.UI.Gtk.Gdk.Keys (keyName)
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
import System.Glib.UTFString (glibToString)

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

data Buffer a = Buffer (Maybe a) (Maybe a) deriving (Show, Eq)

emptyBuffer :: Buffer a
emptyBuffer = Buffer Nothing Nothing

pushBuffer :: a -> Buffer a -> Buffer a
pushBuffer c (Buffer _ b) = Buffer b $ Just c

parseBuffer :: String -> Buffer Char -- xxx hack
parseBuffer [] = Buffer Nothing Nothing
parseBuffer (x:[]) = Buffer Nothing (Just x)
parseBuffer (x:y:[]) = Buffer (Just x) (Just y)
parseBuffer (_:xs) = parseBuffer xs

data Binding = (:==) String (IO ())
infixr 0 :==
type Keymap = [Binding]

fromKeymap :: Keymap -> Buffer Char -> IO ()
fromKeymap raw (Buffer w x) = fromMaybe (return ()) . fmap snd . find (\(Buffer y z,_) -> case y of {
  Nothing -> x == z ;
  Just _  -> w == y && x == z
}) $ map (\(b :== c) -> (parseBuffer b, c)) raw

assignKeymap :: Keymap -> UI -> IO ()
assignKeymap bindings UI{..} = void . after uiWebView keyPressEvent $ do
  pressing <- fmap (head . glibToString . keyName) eventKeyVal -- XXX: can key strings have length > 1?
  liftIO .  (>>= fromKeymap bindings) . atomically $ do
    takeTMVar uiKeyBuffer >>= putTMVar uiKeyBuffer . pushBuffer pressing
    readTMVar uiKeyBuffer 
  return False

keymap :: UI -> Keymap
keymap UI{..} = 
  [ "gg" :== scrollTop uiScrolledWindow
  , "j"  :== scroll uiScrolledWindow Vertical $ Relative 200
  , "k"  :== scroll uiScrolledWindow Vertical $ Relative (-200)
  , "G"  :== scrollBottom uiScrolledWindow
  ]

data UI = UI
  { uiWindow         :: Window
  , uiScrolledWindow :: ScrolledWindow
  , uiWebView        :: WebView
  , uiKeyBuffer      :: TMVar (Buffer Char)
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
    , assignKeymap (keymap ui)
    ]
  getArgs >>= webViewLoadUri uiWebView . head
