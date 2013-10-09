module ENDEC where

import Prelude hiding (readFile, writeFile)
import Data.Char (isAlphaNum)
import Data.ByteString.Char8 (ByteString, pack, readFile, writeFile)
import Data.Functor ((<$>))
import Data.SecureMem (toSecureMem)
import Crypto.Cipher
import Crypto.Cipher.Types

main = do
  doEnc
  return ()

doEnc :: IO ()
doEnc = do
  k <- doInitialize
  e <- doEncode k
  fromRight $ writefile <$> e

fromRight :: Either a b -> b
fromRight (Left a)  = error "error"
fromRight (Right b) = b

doInitialize :: IO (Either KeyError DES)
doInitialize = do
  b <- doReadStream
  --print b
  initialize b

initialize :: ByteString -> IO (Either KeyError DES)
initialize s = do
  let t = takekey <$> toDESKey s
  let x = ciphername t --in print x
  let x = cipherkeysize t --in print x
  return t

doReadStream :: IO ByteString
doReadStream = readfixstr

doEncode :: Either KeyError DES
            -> IO (Either KeyError ByteString)
doEncode key = do
  let e = encode <$> key
  --print e
  return e

encode :: DES -> ByteString
encode = flip ecbEncrypt bs
  where bs = pack "message"

writefile :: ByteString -> IO ()
writefile = writeFile ".\\cipher"

-- buggy
readfile :: IO ByteString
readfile = readFile ".\\passwd.txt"

-- buggy
readline :: IO ByteString
readline = fmap (pack . filter isAlphaNum) getLine

readfixstr :: IO ByteString
readfixstr = (return . pack . filter isAlphaNum) "password"

toDESKey :: ByteString -> Either KeyError (Key DES)
toDESKey = makeKey . toSecureMem

takekey :: Key DES -> DES
takekey = cipherInit

--- * helper for debug print
ciphername = (cipherName <$>)
cipherkeysize = (cipherKeySize <$>)

-- vim: set ts=2 sts=2 sw=2 ai et tw=78:
