module Endec.Wrap (
  readline,
  readfile,
  initializeKey,
  doEncode,
  doDecode,
  StreamFlag(..),
  writeStream,
  ) where

import Prelude hiding (readFile, writeFile)
import Data.Char (isAlphaNum)
import Data.ByteString.Char8 (ByteString, pack, unpack, readFile, writeFile)
import Data.Functor ((<$>))
import Data.SecureMem (toSecureMem)
import Crypto.Cipher
import Crypto.Cipher.Types

initializeKey :: ByteString -> IO (Either KeyError DES)
initializeKey s = do
  let t = takekey <$> toDESKey s
  let x = ciphername t --in print x
  let x = cipherkeysize t --in print x
  return t

doEncode :: ByteString
            -> Either KeyError DES
            -> IO (Either KeyError ByteString)
doEncode msg key = do
  let e = encode msg <$> key
  --print e
  return e

doDecode :: ByteString
            -> Either KeyError DES
            -> IO (Either KeyError ByteString)
doDecode msg key = do
  let d = decode msg <$> key
  --print d
  return d

encode :: ByteString -> DES -> ByteString
encode = flip ecbEncrypt

decode :: ByteString -> DES -> ByteString
decode = flip ecbDecrypt

data StreamFlag = FILE FilePath | STDOUT

writeStream :: StreamFlag -> ByteString -> IO ()
writeStream (FILE f) = writeFile f
writeStream STDOUT   = putStr . filter isAlphaNum . unpack

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
