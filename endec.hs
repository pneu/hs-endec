module Endec (doEnc, doDec) where

import Data.Functor ((<$>))
import Endec.Wrap (
  readline,
  readfile,
  initializeKey,
  doEncode,
  doDecode,
  StreamFlag(..),
  writeStream,
  )

doEnc :: FilePath -> IO ()
doEnc f = do
  kt <- readline
  k <- initializeKey kt
  p <- readline
  c <- doEncode p k
  fromRight $ writeStream (FILE f) <$> c

doDec :: FilePath -> IO ()
doDec f = do
  kt <- readline
  k <- initializeKey kt
  c <- readfile f
  p <- doDecode c k
  fromRight $ writeStream STDOUT <$> p

fromRight :: Either a b -> b
fromRight (Left a)  = error "error"
fromRight (Right b) = b
