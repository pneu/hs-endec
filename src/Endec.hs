module Endec (doEnc, doDec) where

import Data.Functor ((<$>))
import Endec.Wrap (
  initializeKey,
  doEncode,
  doDecode,
  StreamFlag(..),
  readStream,
  writeStream,
  toByteString,
  )

doEnc :: FilePath -> String -> IO ()
doEnc f kt = do
  k <- initializeKey (toByteString kt)
  p <- readStream STDIN
  c <- doEncode p k
  fromRight $ writeStream (FILE f) <$> c

doDec :: FilePath -> String -> IO ()
doDec f kt = do
  k <- initializeKey (toByteString kt)
  c <- readStream (FILE f)
  p <- doDecode c k
  fromRight $ writeStream STDOUT <$> p

fromRight :: Either a b -> b
fromRight (Left _)  = error "error"
fromRight (Right b) = b
