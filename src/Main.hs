module Main where

import           Control.Arrow          (first)
import           Control.Monad          (liftM)
import qualified Crypto.Cipher          as Cipher
import qualified Crypto.Cipher.AES      as AES
import qualified Crypto.Cipher.Types    as CipherTypes
import qualified Cryptopals.Set1        as Set1
import           Cryptopals.Set2
import           Data.Bits              (popCount, xor)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8  as C8
import           Data.Char              (ord)
import           Data.List              (group, sort, transpose, unfoldr)
import qualified Data.List.Key          as K
import           Data.List.Split        (chunksOf)
import qualified Data.Map               as M
import qualified Data.Set               as S
import qualified Data.Word8             as W8

main :: IO ()
main = putStrLn "hi"




