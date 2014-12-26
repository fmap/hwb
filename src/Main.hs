{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TupleSections          #-}

module Main (main) where

import Control.Applicative ((<$>))
import Control.Concurrent.STM.TMVar (TMVar, newTMVar, takeTMVar, putTMVar, readTMVar)
import Control.Error.Util (hoistMaybe)
import Control.Exception (bracket_)
import Control.Monad ((<=<), liftM4, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM (atomically)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.Reader (ReaderT(..), ask, asks)
import Data.HTTPSEverywhere.Rules (rewriteURL)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Graphics.UI.Gtk.Abstract.Container (containerAdd)
import Graphics.UI.Gtk.Abstract.Widget (widgetShowAll, onDestroy)
import qualified Graphics.UI.Gtk.Abstract.Widget as Signal (keyPressEvent)
import Graphics.UI.Gtk.Gdk.EventM (EventM, EKey, eventKeyVal)
import Graphics.UI.Gtk.Gdk.Keys (keyName)
import Graphics.UI.Gtk.General.General (initGUI, mainGUI, mainQuit)
import Graphics.UI.Gtk.Misc.Adjustment (adjustmentGetValue, adjustmentSetValue, adjustmentGetUpper, adjustmentGetLower)
import Graphics.UI.Gtk.Scrolling.ScrolledWindow (ScrolledWindow, scrolledWindowNew, scrolledWindowGetVAdjustment,scrolledWindowGetHAdjustment)
import Graphics.UI.Gtk.WebKit.NetworkRequest (NetworkRequest, networkRequestGetUri, networkRequestSetUri)
import Graphics.UI.Gtk.WebKit.NetworkResponse (NetworkResponse)
import Graphics.UI.Gtk.WebKit.WebFrame (WebFrame)
import Graphics.UI.Gtk.WebKit.WebResource (WebResource)
import Graphics.UI.Gtk.WebKit.WebSettings (WebSettings)
import qualified Graphics.UI.Gtk.WebKit.WebSettings as Attribute (webSettingsEnablePrivateBrowsing, webSettingsEnableScripts)
import Graphics.UI.Gtk.WebKit.WebView (WebView, webViewNew, webViewLoadUri, webViewGetWebSettings, webViewSetWebSettings)
import qualified Graphics.UI.Gtk.WebKit.WebView as Signal (titleChanged, resourceRequestStarting)
import Graphics.UI.Gtk.Windows.Window (Window, windowNew)
import qualified Graphics.UI.Gtk.Windows.Window as Attribute (windowTitle)
import Network.URI (parseURI)
import System.Environment (getArgs)
import System.Glib.Attributes (AttrOp((:=)), set, Attr)
import System.Glib.Signals (Signal, on)
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

class Component a where
  getComponent :: H a

instance Component WebView where
  getComponent = asks uiWebView

instance Component Window where
  getComponent = asks uiWindow

class Component a => Settable a b | a -> b, b -> a where
  getSettings  :: a -> IO b
  setSettings  :: a -> b -> IO ()

instance Settable WebView WebSettings where
  getSettings  = webViewGetWebSettings
  setSettings  = webViewSetWebSettings

instance Settable Window Window where
  getSettings = return
  setSettings = \_ _ -> return ()

is :: Settable a b => Attr b c -> c -> H ()
attribute `is` assigned = getComponent >>= \component -> liftIO $ do
  settings <- getSettings component
  set settings [ attribute := assigned ]
  setSettings component settings

calls :: Component a => Signal a callback -> H callback -> H ()
signal `calls` getCallback = do
  component <- getComponent
  getCallback >>= void . liftIO . on component signal 

setWindowTitle :: H (WebFrame -> String -> IO ())
setWindowTitle = flip runH <$> ask >>= \unH -> return $ \_ title -> unH (windowTitle `is` title)

tryHTTPS :: H (WebFrame -> WebResource -> Maybe NetworkRequest -> Maybe NetworkResponse -> IO ())
tryHTTPS = return $ \_ _ requestM _ -> void . runMaybeT $ do
  (req, uri) <- hoistMaybe requestM <:> hoistMaybe . parseURI <=< MaybeT . networkRequestGetUri
  liftIO $ rewriteURL uri >>= networkRequestSetUri req . show

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

data Keybinding = String :== H ()

infixr 0 :==

fromKeybindings :: [Keybinding] -> Buffer Char -> H ()
fromKeybindings keybindings (Buffer w x) = fromMaybe (return ()) . fmap snd . find (\(Buffer y z,_) -> case y of {
  Nothing -> x == z ;
  Just _  -> w == y && x == z
}) $ map (\(b :== c) -> (parseBuffer b, c)) keybindings

keys :: [Keybinding]
keys =
  [ "gg" :== scrollTop
  , "j"  :== scroll Vertical $ Relative 200
  , "k"  :== scroll Vertical $ Relative (-200)
  , "G"  :== scrollBottom
  ]

tryKeybindings :: [Keybinding] -> H (EventM EKey Bool)
tryKeybindings keybindings= ask >>= \UI{..} -> return $ do
  pressing <- fmap (head . glibToString . keyName) eventKeyVal -- XXX: can key strings have length > 1?
  liftIO . (>>= \buffer -> runH (fromKeybindings keybindings buffer) UI{..}) . atomically $ do
    takeTMVar uiKeyBuffer >>= putTMVar uiKeyBuffer . pushBuffer pressing >> readTMVar uiKeyBuffer
  return False

setURL :: String -> H ()
setURL url = asks uiWebView >>= liftIO . flip webViewLoadUri url

main :: IO ()
main = withUI . runH $ do
  resourceRequestStarting `calls` tryHTTPS
  webSettingsEnableScripts `is` False
  webSettingsEnablePrivateBrowsing `is` True
  titleChanged `calls` setWindowTitle
  keyPressEvent `calls` tryKeybindings keys
  liftIO getArgs >>= setURL . head

------------------------------------------------------------------------

webSettingsEnableScripts :: Attr WebSettings Bool
webSettingsEnableScripts = Attribute.webSettingsEnableScripts

webSettingsEnablePrivateBrowsing :: Attr WebSettings Bool
webSettingsEnablePrivateBrowsing = Attribute.webSettingsEnablePrivateBrowsing

windowTitle :: Attr Window String
windowTitle = Attribute.windowTitle

titleChanged :: Signal WebView (WebFrame -> String -> IO ())
titleChanged = Signal.titleChanged

resourceRequestStarting :: Signal WebView (WebFrame -> WebResource -> Maybe NetworkRequest -> Maybe NetworkResponse -> IO ())
resourceRequestStarting = Signal.resourceRequestStarting

keyPressEvent :: Signal WebView (EventM EKey Bool)
keyPressEvent = Signal.keyPressEvent
--
------------------------------------------------------------------------

(<:>) :: Monad m => m a -> (a -> m b) -> m (a, b)
a <:> g = a >>= \a' -> g a' >>= return . (a', )
infixr 1 <:>

(&) :: a -> (a -> b) -> b
(&) = flip ($)
infixr 0 &
