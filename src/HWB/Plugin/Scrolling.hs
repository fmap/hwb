module HWB.Plugin.Scrolling (
  Orientation(..),
  Position(..),
  scroll,
  scrollBottom,
  scrollTop
) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (asks)
import Graphics.UI.Gtk.Misc.Adjustment (adjustmentGetValue, adjustmentSetValue, adjustmentGetUpper, adjustmentGetLower)
import Graphics.UI.Gtk.Scrolling.ScrolledWindow (scrolledWindowGetVAdjustment,scrolledWindowGetHAdjustment)

import HWB.Core (H)
import HWB.UserInterface (UserInterface(userInterfaceScrolledWindow))

data Orientation = Horizontal | Vertical

data Position a  = Absolute a | Relative a

scroll :: Orientation -> Position Double -> H ()
scroll orientation position = asks userInterfaceScrolledWindow >>= \window -> liftIO $ do
  adjustment <- window & case orientation of 
    Horizontal -> scrolledWindowGetHAdjustment
    Vertical   -> scrolledWindowGetVAdjustment
  adjustmentSetValue adjustment =<< case position of
    Absolute n -> return n
    Relative n -> fmap (+n) $ adjustmentGetValue adjustment

scrollBottom :: H ()
scrollBottom = do
  window   <- asks userInterfaceScrolledWindow
  position <- liftIO $ scrolledWindowGetVAdjustment window >>= adjustmentGetUpper
  scroll Vertical $ Absolute position

scrollTop :: H ()
scrollTop = do
  window   <- asks userInterfaceScrolledWindow
  position <- liftIO $ scrolledWindowGetVAdjustment window >>= adjustmentGetLower
  scroll Vertical $ Absolute position

(&) :: a -> (a -> b) -> b
(&) = flip ($)
infixr 0 &
