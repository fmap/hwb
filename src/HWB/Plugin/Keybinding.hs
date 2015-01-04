{-# LANGUAGE DeriveDataTypeable #-}

module HWB.Plugin.Keybinding (
  Keybinding(..),
  tryKeybindings
) where

import Control.Applicative ((<$>), (<*>), (<$))
import Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.Reader (ask)
import Control.Monad.Trans.State (get)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable)
import Graphics.UI.Gtk.Gdk.EventM (EventM, EKey, eventKeyVal)
import Graphics.UI.Gtk.Gdk.Keys (keyName)
import System.Glib.UTFString (glibToString)
import System.IO.Unsafe (unsafePerformIO)

import HWB.Core (H, runH, ExtensionClass(..))
import qualified HWB.Plugin.Utilities.ExtensibleState as HWB (get)

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

instance ExtensionClass a => ExtensionClass (MVar a) where
  initialValue = unsafePerformIO $ newMVar initialValue -- Hell can't be so bad.. right?

tryKeybindings :: [Keybinding] -> H (EventM EKey Bool)
tryKeybindings keybindings = do
  escape <- runH <$> lift get <*> ask
  return $ False <$ do
    current <- head . glibToString . keyName <$> eventKeyVal
    liftIO . escape $ HWB.get >>= \stored -> do
      buffer <- liftIO $ pushBuffer current <$> takeMVar stored
      liftIO $ putMVar stored buffer 
      fromKeybindings keybindings buffer
