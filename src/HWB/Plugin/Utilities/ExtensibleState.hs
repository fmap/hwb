-- Copyright (c) 2009 Daniel Schoepe
-- 
-- All rights reserved.
-- 
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
-- 
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
-- 
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
-- 
-- 3. Neither the name of the author nor the names of his contributors
--    may be used to endorse or promote products derived from this software
--    without specific prior written permission.
-- 
-- THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS OR
-- IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
-- WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
-- ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
-- STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
-- ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGE.

module HWB.Plugin.Utilities.ExtensibleState (
  get,
  put
) where

import Prelude hiding (lookup)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.State (modify, gets)
import Data.Map (insert, lookup)
import Data.Maybe (fromMaybe)
import Data.Typeable (typeOf, cast)

import HWB.Core (H, ExtensionClass(..), StateExtension(..))

put :: ExtensionClass a => a -> H ()
put value = lift . modify $ insert (show $ typeOf value) (StateExtension value)

get :: ExtensionClass a => H a
get = getState undefined
  where getState :: ExtensionClass a => a -> H a
        getState extensionClassType = fmap (maybe initialValue id . cast . fromMaybe initialValue)
                                    $ lift . gets $ lookup (show $ typeOf extensionClassType)
