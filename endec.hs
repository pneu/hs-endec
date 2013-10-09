module ENDEC (doInitialize, doEnc, doDec) where

import Prelude hiding (readFile, writeFile)
import Data.Char (isAlphaNum)
import Data.ByteString.Char8 (ByteString, pack, unpack, readFile, writeFile)
import Data.Functor ((<$>))
import Control.Applicative ((<*>))
import Data.SecureMem (toSecureMem)
import Crypto.Cipher
import Crypto.Cipher.Types
import System.IO (hPutStrLn, stdout)

data StreamFlag = FILE FilePath | STDOUT

doEnc :: FilePath -> IO ()
doEnc f = do
  putStr "encrypt key: " `seq` return ()  -- don't work strictly
  k <- doInitialize
  m <- readline
  e <- doEncode m k
  fromRight $ (writeStream (FILE f)) <$> e

doDec :: FilePath -> IO ()
doDec f = do
  print "doDec"
  k <- doInitialize
  m <- readfile f
  d <- doDecode m k
  fromRight $ (writeStream STDOUT) <$> d

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
  let e = (encode msg) <$> key
  --print e
  return e

doDecode :: ByteString
            -> Either KeyError DES
            -> IO (Either KeyError ByteString)
doDecode msg key = do
  let d = (decode msg) <$> key
  --print d
  return d

encode :: ByteString -> DES -> ByteString
encode = flip ecbEncrypt

decode :: ByteString -> DES -> ByteString
decode = flip ecbDecrypt

writeStream :: StreamFlag -> ByteString -> IO ()
writeStream (FILE f) = writeFile f
writeStream STDOUT   = (putStr . filter isAlphaNum . unpack)

readfile :: FilePath -> IO ByteString
readfile = readFile

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
