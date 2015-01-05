{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE FunctionalDependencies    #-}

module HWB.Core (
  H,
  runH,
  is,
  calls
) where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT(..), asks)
import Control.Monad.Trans.State (StateT(..))
import Data.Map (Map)
import Graphics.UI.Gtk.WebKit.WebSettings (WebSettings)
import Graphics.UI.Gtk.WebKit.WebView (WebView, webViewGetWebSettings, webViewSetWebSettings)
import Graphics.UI.Gtk.Windows.Window (Window)
import System.Glib.Attributes (AttrOp((:=)), set, Attr)
import System.Glib.Signals (Signal, on)
import XMonad.Core (StateExtension)

import HWB.UserInterface (UserInterface(..))

type H a = ReaderT UserInterface (StateT (Map String StateExtension) IO) a

runH :: Map String StateExtension -> UserInterface -> H a -> IO a
runH st ui h = fmap fst $ runStateT (runReaderT h ui) st

is :: Settable a b => Attr b c -> c -> H ()
attribute `is` assigned = getComponent >>= \component -> liftIO $ do
  settings <- getSettings component
  set settings [ attribute := assigned ]
  setSettings component settings

calls :: Component a => Signal a callback -> H callback -> H ()
signal `calls` getCallback = do
  component <- getComponent
  getCallback >>= void . liftIO . on component signal 

------------------------------------------------------------------------

class Component a where
  getComponent :: H a

instance Component WebView where
  getComponent = asks userInterfaceWebView

instance Component Window where
  getComponent = asks userInterfaceWindow

class Component a => Settable a b | a -> b, b -> a where
  getSettings  :: a -> IO b
  setSettings  :: a -> b -> IO ()

instance Settable WebView WebSettings where
  getSettings  = webViewGetWebSettings
  setSettings  = webViewSetWebSettings

instance Settable Window Window where
  getSettings = return
  setSettings = \_ _ -> return ()
