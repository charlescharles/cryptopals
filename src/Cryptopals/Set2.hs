module Cryptopals.Set2 where

import           Data.Binary            as Binary
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8  as C8
import qualified Data.Word8             as W8


-- Ch 9

--pkcsPad :: Int -> BS.ByteString -> BS.ByteString
--pkcsPad n s = BS.append s padding where
  --  diff = n - BS.length

