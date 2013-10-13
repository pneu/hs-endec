module Main where
import Endec (doEnc, doDec)

main = do
  doEnc "cipher"
  doDec "cipher"
  return ()
