{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}

module Main (main) where

import Control.Concurrent.STM.TMVar (TMVar, newTMVar, takeTMVar, putTMVar, readTMVar)
import Control.Error.Util (hoistMaybe)
import Control.Exception (bracket_)
import Control.Monad (liftM4, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM (atomically)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.Reader (ReaderT(..), ask, asks)
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
import System.Glib.Attributes (AttrOp((:=)), set, Attr)
import System.Glib.Signals (on, after)
import System.Glib.UTFString (glibToString)

data UI = UI
  { uiWindow         :: Window
  , uiScrolledWindow :: ScrolledWindow
  , uiWebView        :: WebView
  , uiKeyBuffer      :: TMVar (Buffer Char)
  }

type H a = ReaderT UI IO a

runH :: H a -> UI -> IO a
runH = runReaderT

withUI :: (UI -> IO a) -> IO ()
withUI callback = bracket_ initGUI mainGUI . void $ do
  ui@UI{..} <- liftM4 UI windowNew (scrolledWindowNew Nothing Nothing) webViewNew (atomically $ newTMVar emptyBuffer)
  uiWindow `containerAdd` uiScrolledWindow
  uiScrolledWindow `containerAdd` uiWebView
  _ <- callback ui
  widgetShowAll uiWindow >> onDestroy uiWindow mainQuit

httpsEverywhere :: H ()
httpsEverywhere = do
  UI{..} <- ask;
  liftIO . void . on uiWebView resourceRequestStarting $ \_ _ requestM _ -> void . runMaybeT $ do
    request  <- hoistMaybe requestM
    httpsURI <- MaybeT (networkRequestGetUri request) >>= hoistMaybe . parseURI >>= liftIO . rewriteURL
    liftIO . networkRequestSetUri request $ show httpsURI

(&) :: a -> (a -> b) -> b
(&) = flip ($)
infixr 0 &

data Orientation = Horizontal | Vertical

data Position a  = Absolute a | Relative a

scroll :: Orientation -> Position Double -> H ()
scroll orientation position = asks uiScrolledWindow >>= \window -> liftIO $ do
  adjustment <- window & case orientation of 
    Horizontal -> scrolledWindowGetHAdjustment
    Vertical   -> scrolledWindowGetVAdjustment
  adjustmentSetValue adjustment =<< case position of
    Absolute n -> return n
    Relative n -> fmap (+n) $ adjustmentGetValue adjustment

scrollBottom :: H ()
scrollBottom = do
  window   <- asks uiScrolledWindow
  position <- liftIO $ scrolledWindowGetVAdjustment window >>= adjustmentGetUpper
  scroll Vertical $ Absolute position

scrollTop :: H ()
scrollTop = do
  window   <- asks uiScrolledWindow
  position <- liftIO $ scrolledWindowGetVAdjustment window >>= adjustmentGetLower
  scroll Vertical $ Absolute position

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

data Binding = String :== H ()
infixr 0 :==
type Keymap = [Binding]

fromKeymap :: Keymap -> Buffer Char -> H ()
fromKeymap raw (Buffer w x) = fromMaybe (return ()) . fmap snd . find (\(Buffer y z,_) -> case y of {
  Nothing -> x == z ;
  Just _  -> w == y && x == z
}) $ map (\(b :== c) -> (parseBuffer b, c)) raw

assignKeymap :: Keymap -> H ()
assignKeymap bindings = do
  UI{..} <- ask
  liftIO . void . after uiWebView keyPressEvent $ do
    pressing <- fmap (head . glibToString . keyName) eventKeyVal -- XXX: can key strings have length > 1?
    liftIO . (>>= \buffer -> runH (fromKeymap bindings buffer) UI{..}) . atomically $ do
      takeTMVar uiKeyBuffer >>= putTMVar uiKeyBuffer . pushBuffer pressing
      readTMVar uiKeyBuffer
    return False

keymap :: Keymap
keymap =
  [ "gg" :== scrollTop
  , "j"  :== scroll Vertical $ Relative 200
  , "k"  :== scroll Vertical $ Relative (-200)
  , "G"  :== scrollBottom
  ]

labelWindow :: H ()
labelWindow = do
  UI{..} <- ask 
  liftIO . void . on uiWebView titleChanged $ \_ title -> flip runH UI{..} $ do
    (windowTitle :: Attr Window String) `is` title

class Settable a b | a -> b, b -> a where
  getComponent :: H a
  getSettings  :: a -> IO b
  setSettings  :: a -> b -> IO ()

instance Settable WebView WebSettings where
  getComponent = asks uiWebView
  getSettings  = webViewGetWebSettings
  setSettings  = webViewSetWebSettings

instance Settable Window Window where
  getComponent = asks uiWindow
  getSettings = return
  setSettings = \_ _ -> return ()

is :: Settable a b => Attr b c -> c -> H ()
attribute `is` assigned = getComponent >>= \component -> liftIO $ do
  settings <- getSettings component
  set settings [ attribute := assigned ]
  setSettings component settings

setURL :: String -> H ()
setURL url = asks uiWebView >>= liftIO . flip webViewLoadUri url

main :: IO ()
main = withUI . runH $ do
  httpsEverywhere
  (webSettingsEnableScripts :: Attr WebSettings Bool) `is` False
  (webSettingsEnablePrivateBrowsing :: Attr WebSettings Bool) `is` True
  labelWindow
  assignKeymap keymap
  liftIO getArgs >>= setURL . head
