--module ENDEC (endec) where

import Prelude hiding (readFile, writeFile)
import Data.Char (isAlphaNum)
import Data.ByteString.Char8 (ByteString, pack, unpack, readFile, writeFile)
import Data.Functor ((<$>))
import Control.Applicative ((<*>))
import Data.SecureMem (toSecureMem)
import Crypto.Cipher
import Crypto.Cipher.Types

main = do
  k <- doInitialize
  c <- doEncode k
  let w = writefile <$> c
  p <- doDecode k c
  fromRight w

fromRight :: Either a b -> b
fromRight (Left a)  = error "failed to encode"
fromRight (Right b) = b

initialize :: ByteString -> IO (Either KeyError DES)
initialize s = do
  let t = takekey <$> toDESKey s
  let x = ciphername t --in print x
  let x = cipherkeysize t --in print x
  return t

doInitialize :: IO (Either KeyError DES)
doInitialize = do
  b <- doReadfile
  --print b
  initialize b

doReadfile :: IO ByteString
doReadfile = readfixstr

doEncode :: Either KeyError DES -> IO (Either KeyError ByteString)
doEncode key = do
  let c = encode <$> key
  --print c
  return c

doDecode :: Either KeyError DES
            -> Either KeyError ByteString
            -> IO (Either KeyError ByteString)
doDecode k a = do
  let e = decode <$> k <*> a
  --print e
  return e

encode :: DES -> ByteString
encode = flip ecbEncrypt (pack "ciphered")

decode :: DES -> ByteString -> ByteString
decode = ecbDecrypt

writefile :: ByteString -> IO ()
writefile = writeFile ".\\ciphered.txt"

-- buggy
readfile :: IO ByteString
readfile = readFile ".\\passwd.txt"

-- buggy
readline :: IO ByteString
readline = fmap (pack . filter isAlphaNum) getLine

readfixstr :: IO ByteString
readfixstr = (return . pack . filter isAlphaNum) "password"

toDESKey :: ByteString -> Either KeyError (Key DES)
--toDESKey = makeKey . toSecureMem
toDESKey = makeKey

takekey :: Key DES -> DES
takekey = cipherInit

--- * helper for debug print
ciphername = (cipherName <$>)
cipherkeysize = (cipherKeySize <$>)

-- vim: set ts=2 sts=2 sw=2 ai et tw=78:
