{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TupleSections             #-}

module Main (main) where

import Prelude hiding (lookup)
import Control.Applicative ((<$>), (<*>), (<$))
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar(..), newMVar, putMVar, takeMVar)
import Control.Error.Util (hoistMaybe)
import Control.Exception (bracket_)
import Control.Monad ((<=<), liftM3, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.Reader (ReaderT(..), ask, asks)
import Control.Monad.Trans.State (StateT(..), get, gets, put, modify)
import Data.HTTPSEverywhere.Rules (rewriteURL)
import Data.List (find)
import Data.Map (Map(..), empty, lookup, insert)
import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable(..), typeOf, cast)
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
import System.IO.Unsafe (unsafePerformIO)

data UI = UI
  { uiWindow         :: Window
  , uiScrolledWindow :: ScrolledWindow
  , uiWebView        :: WebView
  }
 
---------------------------------------------------
--
-- Copyright   :  (c) Daniel Schoepe 2009
-- License     :  BSD3-style (see LICENSE)
--
-- << miLRtiFuUw

class Typeable a => ExtensionClass a where
  initialValue :: a

data StateExtension = forall a. ExtensionClass a => StateExtension a
  deriving Typeable

instance ExtensionClass StateExtension where
  initialValue = initialValue

type State = Map String StateExtension

putH :: ExtensionClass a => a -> H ()
putH value = lift . modify $ insert (show $ typeOf value) (StateExtension value)

getH :: ExtensionClass a => H a
getH = getState undefined
  where getState :: ExtensionClass a => a -> H a
        getState extensionClassType = fmap (maybe initialValue id . cast . fromMaybe initialValue)
                                    $ lift . gets $ lookup (show $ typeOf extensionClassType)

---  miLRtiFuUw -------------------------------------

type H a = ReaderT UI (StateT State IO) a

runH :: State -> UI -> H a -> IO (a, State)
runH st ui h = runStateT (runReaderT h ui) st

withUI :: (UI -> IO a) -> IO ()
withUI callback = bracket_ initGUI mainGUI . void $ do
  ui@UI{..} <- liftM3 UI windowNew (scrolledWindowNew Nothing Nothing) webViewNew
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
setWindowTitle = runH <$> lift get <*> ask >>= \unH ->
  return $ \_ title -> void . unH $ windowTitle `is` title

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

data Buffer a = Buffer (Maybe a) (Maybe a) deriving (Show, Eq, Typeable)

instance Typeable a => ExtensionClass (Buffer a) where
  initialValue = Buffer Nothing Nothing

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

instance ExtensionClass a => ExtensionClass (MVar a) where
  initialValue = unsafePerformIO $ newMVar initialValue -- Hell can't be so bad.. right?

tryKeybindings :: [Keybinding] -> H (EventM EKey Bool)
tryKeybindings keybindings = do
  escape <- runH <$> lift get <*> ask
  return $ False <$ do
    current <- head . glibToString . keyName <$> eventKeyVal
    liftIO . escape $ getH >>= \stored -> do
      buffer <- liftIO $ pushBuffer current <$> takeMVar stored
      liftIO $ putMVar stored buffer 
      fromKeybindings keybindings buffer

setURL :: String -> H ()
setURL url = asks uiWebView >>= liftIO . flip webViewLoadUri url

main :: IO ()
main = withUI . flip (runH empty) $ do
  webSettingsEnableScripts `is` False
  webSettingsEnablePrivateBrowsing `is` True
  resourceRequestStarting `calls` tryHTTPS
  titleChanged `calls` setWindowTitle
  keyPressEvent `calls` tryKeybindings keys
  liftIO getArgs >>= setURL . head

--------------------------------------------------------------------------

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

--------------------------------------------------------------------------

(<:>) :: Monad m => m a -> (a -> m b) -> m (a, b)
a <:> g = a >>= \a' -> g a' >>= return . (a', )
infixr 1 <:>

(&) :: a -> (a -> b) -> b
(&) = flip ($)
infixr 0 &
