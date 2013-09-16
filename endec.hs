module ENDEC where

import Prelude hiding (readFile)
import Data.Char (isAlphaNum)
import Data.ByteString.Char8 (ByteString, pack, readFile)
import Data.Functor ((<$>))
import Data.SecureMem (toSecureMem)
import Crypto.Cipher
import Crypto.Cipher.Types

--main = undefined
main = do
  key <- initialize
  encode key
  return ()

initialize :: IO (Either KeyError DES)
initialize = do
  --s <- readfile
  --s <- readline
  s <- readfixstr
  let t = (takekey . toDESKey) s
  let x = ciphername t in print x
  let x = cipherkeysize t in print x
  return t

encode = undefined

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

takekey a = cipherInit <$> a

--- * helper for debug print
ciphername a = cipherName <$> a
cipherkeysize a = cipherKeySize <$> a

-- vim: set ts=2 sts=2 sw=2 ai et tw=78:
