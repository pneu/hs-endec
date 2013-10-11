module ENDEC where

import Prelude hiding (readFile, writeFile)
import Data.Char (isAlphaNum)
import Data.ByteString.Char8 (ByteString, pack, readFile, writeFile)
import Data.Functor ((<$>))
import Data.SecureMem (toSecureMem)
import Crypto.Cipher
import Crypto.Cipher.Types

main = do
  doEnc "cipher"
  return ()

doEnc :: FilePath -> IO ()
doEnc f = do
  k <- doInitialize
  m <- readline
  e <- doEncode m k
  fromRight $ writefile f <$> e

fromRight :: Either a b -> b
fromRight (Left a)  = error "error"
fromRight (Right b) = b

doInitialize :: IO (Either KeyError DES)
doInitialize = do
  b <- doReadStream
  --print b
  initializeKey b

initializeKey :: ByteString -> IO (Either KeyError DES)
initializeKey s = do
  let t = takekey <$> toDESKey s
  let x = ciphername t --in print x
  let x = cipherkeysize t --in print x
  return t

doReadStream :: IO ByteString
doReadStream = readline

doEncode :: ByteString
            -> Either KeyError DES
            -> IO (Either KeyError ByteString)
doEncode msg key = do
  let e = encode msg <$> key
  --print e
  return e

encode :: ByteString -> DES -> ByteString
encode = flip ecbEncrypt

writefile :: FilePath -> ByteString -> IO ()
writefile = writeFile

readfile :: FilePath -> IO ByteString
readfile = readFile

readline :: IO ByteString
readline = fmap (pack . filter isAlphaNum) getLine

toDESKey :: ByteString -> Either KeyError (Key DES)
toDESKey = makeKey . toSecureMem

takekey :: Key DES -> DES
takekey = cipherInit

--- * helper for debug print
ciphername = (cipherName <$>)
cipherkeysize = (cipherKeySize <$>)

-- vim: set ts=2 sts=2 sw=2 ai et tw=78:
