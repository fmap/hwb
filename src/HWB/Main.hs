module HWB.Main (
  hwb
) where

import Data.Map (empty)

import HWB.Core (H, runH)
import HWB.UserInterface (withUserInterface)

hwb :: H a -> IO ()
hwb = withUserInterface . flip (runH empty)
