{-# LANGUAGE FlexibleInstances #-}
module ENDEC where

import Prelude hiding (readFile, writeFile)
import Data.Char (isAlphaNum)
import Data.ByteString.Char8 (ByteString, pack, unpack, readFile, writeFile, hPut)
import Data.Functor ((<$>))
import qualified Data.ByteString.Base64 as B64 (encode, decode)
import Control.Applicative (pure, (<*>))
import Control.DeepSeq (($!!), NFData(..))
import Data.SecureMem (toSecureMem)
import Crypto.Cipher
import Crypto.Cipher.Types
import System.IO (withBinaryFile, IOMode(WriteMode), hSetBinaryMode)

instance NFData KeyError
instance NFData (IO ())

main = do
  b <- doReadfile
  print b
  k <- initialize b
  let c = encode <$> k
  print c
  let a = B64.encode <$> c
  print a
  --let d = writefile <$> a
  let d = id $!! (writefile <$> a)
  --let x = id $!! d
  --let e = decode <$> k <*> c
  let g = B64.decode <$> a
  print g
  let e = decode' k g
  print e
  fromRight d

fromRight :: Either a b -> b
fromRight (Left a)  = error "failed to encode"
fromRight (Right b) = b

doReadfile :: IO ByteString
doReadfile = readfixstr

initialize :: ByteString -> IO (Either KeyError DES)
initialize s = do
  let t = takekey <$> toDESKey s
  let x = ciphername t in print x
  let x = cipherkeysize t in print x
  return t

encode :: DES -> ByteString
encode = flip ecbEncrypt (pack "ciphered")

decode :: DES -> ByteString -> ByteString
decode = ecbDecrypt

decode' :: Either KeyError DES
            -> Either KeyError (Either String ByteString)
            -> Either KeyError (Either String ByteString)
decode' k g = case k of
                Left ke -> error "error key"
                Right k' -> case g of
                    Left e -> error "error"
                    Right a -> case a of
                        Left e' -> error "error2"
                        Right a' -> return $ return $ ecbDecrypt k' a'

writefile :: ByteString -> IO ()
writefile = writeFile ".\\ciphered.txt"

eval a = a `seq` ()

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
