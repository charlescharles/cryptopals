{-# LANGUAGE BangPatterns #-}

module Cryptopals.Set1 where

import           Control.Arrow          (first)
import           Control.Monad          (liftM)
import qualified Crypto.Cipher          as Cipher
import qualified Crypto.Cipher.AES      as AES
import qualified Crypto.Cipher.Types    as CipherTypes
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

rankBy :: (BS.ByteString -> Int) -> [BS.ByteString] -> [BS.ByteString]
rankBy score = K.sort score

possibilities :: BS.ByteString -> [BS.ByteString]
possibilities s = map (`xorSingle` s) [minBound..maxBound]

decodeXor :: String -> String
decodeXor = C8.unpack . head . rankBy score . possibilities . fromHex

decodeXorBS :: BS.ByteString -> BS.ByteString
decodeXorBS = head . rankBy score . possibilities

-- Ch 4
type Histogram = M.Map W8.Word8 Int

englishFrequencies' :: [(Char, Float)]
englishFrequencies' = [ (' ', 0.183)
                     , ('e', 0.103)
                     , ('t', 0.075)
                     , ('a', 0.065)
                     , ('o', 0.062)
                     , ('n', 0.057)
                     , ('i', 0.057)
                     , ('s', 0.053)
                     , ('r', 0.050)
                     , ('h', 0.050)
                     , ('l', 0.033) ]

englishFrequencies :: M.Map W8.Word8 Float
englishFrequencies = M.fromList $ map (first (fromIntegral . ord)) englishFrequencies'

score :: BS.ByteString -> Int
score s = histogramDiff histo engHisto where
    n = BS.length s
    scale x = round $ (fromIntegral n) * x
    engHisto = M.map scale englishFrequencies
    histo = histogram (BS.map W8.toLower s)

histogramDiff :: Histogram -> Histogram -> Int
histogramDiff a b = M.fold (+) 0 $ M.unionWith f a b where
    f x y = abs (x - y)

histogram :: BS.ByteString -> Histogram
histogram = BS.foldr' f M.empty where
    f w = M.insertWith' (+) w 1

decodeAll :: [String] -> String
decodeAll = C8.unpack . head . rankBy score . map (C8.pack . decodeXor)

-- Ch 5
xorEncrypt :: String -> String -> String
xorEncrypt text k = (C8.unpack . B16.encode . BS.pack) (BS.zipWith xor bsText (rkey)) where
    bsText = C8.pack text
    rkey = C8.pack $ take (length text) (concat $ repeat k)

-- Ch 6
dist :: BS.ByteString -> BS.ByteString -> Int
dist s = sum . map popCount . BS.zipWith xor s

pairDist :: [BS.ByteString] -> Int
pairDist [a, b] = dist a b
pairDist _ = 0

hammingCost :: BS.ByteString -> Int -> Int
hammingCost s n = (sum . map pairDist . chunksOf 2 . map BS.pack . chunksOf n . BS.unpack) s

bestKeysizes :: BS.ByteString -> Int -> [Int]
bestKeysizes s n = take n $ K.sort (hammingCost s) [2..40]

-- break ciphertext into blocks of length n
makeBlocks :: Int -> BS.ByteString -> [BS.ByteString]
makeBlocks n = unfoldr f where
    f s = let t@(a, _) = BS.splitAt n s in
          if BS.null a
             then Nothing
             else Just t

transposeBlocks :: [BS.ByteString] -> [BS.ByteString]
transposeBlocks = map BS.pack . transpose . map BS.unpack

solveWithKeysize :: BS.ByteString -> Int -> String
solveWithKeysize s n = (C8.unpack . BS.concat . transposeBlocks . map decodeXorBS . transposeBlocks . makeBlocks n) s

cleanRead64 :: FilePath -> IO BS.ByteString
cleanRead64 f = BS.readFile f >>= return . B64.decodeLenient . C8.filter (not . (== '\n'))

bestDecryption :: [String] -> String
bestDecryption = C8.unpack . head . rankBy score . map C8.pack

solve6 :: BS.ByteString -> String
solve6 s = bestDecryption . map (solveWithKeysize s) $ bestKeysizes s 5

-- Ch 7

key7 :: AES.AES
key7 = AES.initAES (C8.pack "YELLOW SUBMARINE")

decrypt7 :: BS.ByteString -> BS.ByteString
decrypt7 = (key7 `AES.decryptECB`)

-- Ch 8

-- count 16-byte block repetitions in hex string
-- 1 byte = 2 hex chars
blockRepetitions :: String -> Int
blockRepetitions = length . K.maximum length . group . sort . chunksOf 32

solve8 :: String -> String
solve8 = K.maximum blockRepetitions . lines

-- all challenges
set1 :: [IO String]
set1 = [ return $ hexToBase64 "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d",
          return $ xorHex "1c0111001f010100061a024b53535009181c" "686974207468652062756c6c277320657965",
          return $ decodeXor "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736",
--         readFile "src/s1c4.txt" >>= return . decodeAll . lines,
         return $ xorEncrypt "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal" "ICE",
         cleanRead64 "src/6.txt" >>= return . solve6,
         solve8 `liftM` readFile "src/8.txt"]


main :: IO ()
main = do
  let display (i, s) = putStrLn ("\tChallenge " ++ show i ++ ":\n\t>>" ++ s ++ "\n")
  putStrLn "Set 1:"
  challenges <- sequence set1
  mapM_ display (zip [1..] challenges)


