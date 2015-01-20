module Main where

import           Data.Bits              (xor)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8  as C8
import           Data.Char              (ord)
import           Data.List              (sortBy)
import qualified Data.Word8             as W8

-- Set 1

-- Ch 1
hexToBase64 :: String -> String
hexToBase64 = C8.unpack . B64.encode . fst . B16.decode . C8.pack

-- Ch 2
xorHex :: String -> String -> String
xorHex a b = (C8.unpack . B16.encode . BS.pack) $ BS.zipWith xor p q where
                        (p, q) = (f a, f b)
                        f = fst . B16.decode . C8.pack

-- Ch 3
charToWord8 :: Char -> W8.Word8
charToWord8 = fromIntegral . ord

fromHex :: String -> BS.ByteString
fromHex = fst . B16.decode . C8.pack

xorSingle :: W8.Word8 -> BS.ByteString -> BS.ByteString
xorSingle w = BS.map (xor w)

scoreByChar :: (W8.Word8 -> Int) -> BS.ByteString -> Int
scoreByChar f = sum . map f . BS.unpack

englishScore :: W8.Word8 -> Int
englishScore = fromEnum . (`elem` english) where
    english = map charToWord8 [' '..'z']

alphaScore :: W8.Word8 -> Int
alphaScore c = fromEnum (W8.isAscii c && W8.isAlpha c)

rankBy :: (BS.ByteString -> Int) -> [BS.ByteString] -> [BS.ByteString]
rankBy score = reverse . sortBy f where
		f a b = compare (score a) (score b)

possibilities :: BS.ByteString -> [BS.ByteString]
possibilities s = map (`xorSingle` s) [minBound..maxBound]

score3 :: W8.Word8 -> Int
score3 c = englishScore c + alphaScore c

decodeXor :: String -> String
decodeXor = C8.unpack . head . rankBy score . possibilities . fromHex where
    score = scoreByChar score3

-- Ch 4
--decode4 :: [String] -> String

-- Ch 5
xorEncrypt :: String -> String -> String
xorEncrypt text k = (C8.unpack . B16.encode . BS.pack) (BS.zipWith xor bsText (rkey)) where
    bsText = C8.pack text
    rkey = C8.pack $ take (length text) (concat $ repeat k)


-- all challenges
set1 :: [String]
set1 = [ hexToBase64 "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d",
          xorHex "1c0111001f010100061a024b53535009181c" "686974207468652062756c6c277320657965",
          decodeXor "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736" ]

main :: IO ()
main = do
  let display (i, s) = putStrLn ("\tChallenge " ++ show i ++ ":\n\t>>" ++ s ++ "\n")
  putStrLn "Set 1:"
  mapM_ display (zip [1..] set1)


