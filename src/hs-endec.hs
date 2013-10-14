module Main where
import Endec (doEnc, doDec)

main :: IO ()
main = do
  doEnc "cipher" "password"
  doDec "cipher" "password"
  return ()
