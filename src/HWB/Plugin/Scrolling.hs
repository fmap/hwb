module HWB.Plugin.Scrolling (
  Orientation(..),
  Position(..),
  scroll,
  scrollBottom,
  scrollTop,
  pageUp,
  pageDown
) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (asks)
import Graphics.UI.Gtk.Misc.Adjustment (Adjustment, adjustmentGetValue, adjustmentSetValue, adjustmentGetUpper, adjustmentGetLower, adjustmentGetPageSize)
import Graphics.UI.Gtk.Scrolling.ScrolledWindow (scrolledWindowGetVAdjustment,scrolledWindowGetHAdjustment)

import HWB.Core (H)
import HWB.UserInterface (UserInterface(userInterfaceScrolledWindow))

data Orientation = Horizontal | Vertical

data Position a  = Absolute a | Relative a

withAdjustment :: Orientation -> (Adjustment -> IO a) -> H a
withAdjustment orientation callback = do
  window <- asks userInterfaceScrolledWindow
  adjust <- window & liftIO . case orientation of
    Horizontal -> scrolledWindowGetHAdjustment
    Vertical   -> scrolledWindowGetVAdjustment
  liftIO $ callback adjust

scroll :: Orientation -> Position Double -> H ()
scroll orientation position = withAdjustment orientation $ \adjustment ->
  adjustmentSetValue adjustment =<< case position of
    Absolute n -> return n
    Relative n -> fmap (+n) $ adjustmentGetValue adjustment

page' :: (Double -> Double) -> H ()
page' modifier = withAdjustment Vertical adjustmentGetPageSize
             >>= scroll Vertical . Relative . modifier

pageUp, pageDown :: H ()
pageUp = page' negate
pageDown = page' id

scrollBottom :: H ()
scrollBottom = withAdjustment Vertical adjustmentGetUpper
           >>= scroll Vertical . Absolute

scrollTop :: H ()
scrollTop = withAdjustment Vertical adjustmentGetLower
        >>= scroll Vertical . Absolute

(&) :: a -> (a -> b) -> b
(&) = flip ($)
infixr 0 &
