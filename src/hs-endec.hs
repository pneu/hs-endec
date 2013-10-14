module Main where
import Endec (doEnc, doDec)
import Data.Maybe (fromMaybe)
import System.Console.GetOpt (
  usageInfo, getOpt,
  ArgOrder(Permute), OptDescr(Option), ArgDescr(NoArg, ReqArg, OptArg))
import System.IO (hPutStrLn, stderr)
import System.Environment (getArgs)
import System.Exit (exitFailure)

main :: IO ()
main = do
  a <- getArgs
  if length a <= 0
    then do
      hPutStrLn stderr "Arguments required"
      usageAction
    else do
      o <- parseOpt a
      print o
      uncurry analyseOpt o
      return ()

data Flag = Encoding | Decoding | Key String | Filename FilePath | Help
  deriving (Show, Eq)

options :: [OptDescr Flag]
options =
  [ Option "e" ["encode"] (NoArg Encoding)  "encode"
  , Option "d" ["decode"] (NoArg Decoding)  "decode"
  , Option "k" ["key"]    (ReqArg Key "Key") "key"
  , Option "f" ["file"]   (OptArg mkFilename "FILE") "input/output encrypted file (default: cipher)"
  , Option "h" ["help"]   (NoArg Help)      "show this help and exit"
  ]

mkFilename :: Maybe FilePath -> Flag
mkFilename = Filename . fromMaybe defaultFilename

defaultFilename :: FilePath
defaultFilename = "cipher"

parseOpt :: [String] -> IO ([Flag], [String])
parseOpt argv = case getOpt Permute options argv of
  (o, n, [])  -> return (o, n)
  (_, _, e)   -> ioError (userError (concat e ++ usage))

analyseOpt :: [Flag] -> [String] -> IO ()
analyseOpt [] _ = usageAction
analyseOpt os@(x:xs) _ = if Help `elem` os
                        then usageAction
                        else getAction x xs

usageAction :: IO ()
usageAction = do
  putStrLn usage
  exitFailure

usage :: String
usage = usageInfo header options
  where header = "Usage: endec [OPTION]"

getAction :: Flag -> [Flag] -> IO ()
getAction Encoding xs            = doEnc (getFilename xs) (getKey xs)
getAction Decoding xs            = doDec (getFilename xs) (getKey xs)
getAction (Key key) (x:xs)       = getAction x (Key key:xs)
getAction (Filename path) (x:xs) = getAction x (Filename path:xs)
getAction _ _                    = usageAction

getKey :: [Flag] -> String
getKey (Key x:_) = x
getKey (_:xs) = getKey xs

getFilename :: [Flag] -> FilePath
getFilename [] = defaultFilename
getFilename (o:os) = isFilename o
  where
    isFilename (Filename s) = s
    isFilename _  = getFilename os
