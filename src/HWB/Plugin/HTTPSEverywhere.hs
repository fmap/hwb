{-# LANGUAGE TupleSections #-}

module HWB.Plugin.HTTPSEverywhere (
  tryHTTPS
) where

import Control.Error.Util (hoistMaybe)
import Control.Monad ((<=<), void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.HTTPSEverywhere.Rules (rewriteURL)
import Graphics.UI.Gtk.WebKit.NetworkRequest (NetworkRequest, networkRequestGetUri, networkRequestSetUri)
import Graphics.UI.Gtk.WebKit.NetworkResponse (NetworkResponse)
import Graphics.UI.Gtk.WebKit.WebFrame (WebFrame)
import Graphics.UI.Gtk.WebKit.WebResource (WebResource)
import Network.URI (parseURI)

import HWB.Core (H)

tryHTTPS :: H (WebFrame -> WebResource -> Maybe NetworkRequest -> Maybe NetworkResponse -> IO ())
tryHTTPS = return $ \_ _ requestM _ -> void . runMaybeT $ do
  (req, uri) <- hoistMaybe requestM <:> hoistMaybe . parseURI <=< MaybeT . networkRequestGetUri
  liftIO $ rewriteURL uri >>= networkRequestSetUri req . show

(<:>) :: Monad m => m a -> (a -> m b) -> m (a, b)
a <:> g = a >>= \a' -> g a' >>= return . (a', )
infixr 1 <:>
