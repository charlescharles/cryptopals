{-# LANGUAGE BangPatterns #-}

module Main where

import           Control.Arrow          (first)
import           Data.Bits              (popCount, xor)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8  as C8
import           Data.Char              (ord)
import           Data.List              (transpose, unfoldr)
import           Data.List.Key          (sort)
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
rankBy score = reverse . sort score

possibilities :: BS.ByteString -> [BS.ByteString]
possibilities s = map (`xorSingle` s) [minBound..maxBound]

decodeXor :: String -> String
decodeXor = C8.unpack . head . rankBy score . possibilities . fromHex

decodeXorBS :: BS.ByteString -> BS.ByteString
decodeXorBS = head . rankBy score . possibilities

-- Ch 4
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
score s = M.foldrWithKey f 0 fr where
    n = BS.length s
    fr = freqs (BS.map W8.toLower s)
    f ch ct acc = case M.lookup ch englishFrequencies of
                    Nothing -> -ct + acc
                    Just frac -> -(absDiff frac ct) + acc
    absDiff :: Float -> Int -> Int
    absDiff p ct = abs $ round (p * (fromIntegral n)) - ct

freqs :: BS.ByteString -> M.Map W8.Word8 Int
freqs = BS.foldr' f M.empty where
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

first2chunks :: Int -> BS.ByteString -> (BS.ByteString, BS.ByteString)
first2chunks n s = (a, b) where
    (a, rest) = BS.splitAt n s
    b = BS.take n rest

-- bytestring is the total text, int is keysize, float is
-- (hamming dist between first/second chunk of n bytes)/keysize
normalized2delta :: BS.ByteString -> Int -> Float
normalized2delta s n = (fromIntegral $ dist a b) / (fromIntegral n) where
    (a, b) = first2chunks n s

bestKeysizes :: BS.ByteString -> Int -> [Int]
bestKeysizes s n = take n $ sort (normalized2delta s) [2..40]

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

makeBlocks' :: Int -> String -> [String]
makeBlocks' n = unfoldr f where
    f s = let t@(a, _) = splitAt n s in
          if null a
             then Nothing
             else Just t


-- all challenges
set1 :: [IO String]
set1 = [ return $ hexToBase64 "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d",
          return $ xorHex "1c0111001f010100061a024b53535009181c" "686974207468652062756c6c277320657965",
          return $ decodeXor "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736",
--         readFile "src/s1c4.txt" >>= return . decodeAll . lines,
         return $ xorEncrypt "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal" "ICE"]


main :: IO ()
main = do
  let display (i, s) = putStrLn ("\tChallenge " ++ show i ++ ":\n\t>>" ++ s ++ "\n")
  putStrLn "Set 1:"
  challenges <- sequence set1
  mapM_ display (zip [1..] challenges)


