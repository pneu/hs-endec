module ENDEC where

import Prelude hiding (readFile)
import Data.Char (isAlphaNum)
import Data.ByteString.Char8 (ByteString, pack, readFile)
import Data.Functor ((<$>))
import Data.SecureMem (toSecureMem)
import Crypto.Cipher
import Crypto.Cipher.Types

main = do
  key <- initialize
  let w = encode <$> key
  --print w
  return ()

initialize :: IO (Either KeyError DES)
initialize = do
  --s <- readfile
  --s <- readline
  s <- readfixstr
  let t = takekey <$> toDESKey s
  let x = ciphername t --in print x
  let x = cipherkeysize t --in print x
  return t

encode :: DES -> ByteString
encode = flip ecbEncrypt bs
  where bs = pack "message"

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
