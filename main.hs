import ENDEC (doInitialize, doEnc, doDec)
import Data.Maybe (fromMaybe)
import System.Console.GetOpt (
  usageInfo, getOpt,
  ArgOrder(Permute), OptDescr(Option), ArgDescr(NoArg, OptArg))
import System.IO (hPutStrLn, stderr)
import System.Environment (getArgs)
import System.Exit (exitFailure)

main = do
  a <- getArgs
  if length a <= 0
    then do
      hPutStrLn stderr "Arguments required"
      usageAction
      exitFailure
    else do
      o <- parseOpt a
      print o
      x <- analyseOpt (fst o) (snd o)
      return ()

data Flag = Encoding | Decoding | Filename FilePath | Help
  deriving (Show)

options :: [OptDescr Flag]
options =
  [ Option "e" ["encode"] (NoArg Encoding)  "encode"
  , Option "d" ["decode"] (NoArg Decoding)  "decode"
  , Option "f" ["file"]   (OptArg mkFilename "FILE") "input/output encrypted file (default: cipher)"
  , Option "h" ["help"]   (NoArg Help)      "DES encoder/decoder"
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
analyseOpt xs@(o:os) _ = getAction o os

usageAction :: IO ()
usageAction = do
  putStrLn usage

usage :: String
usage = usageInfo header options
  where header = "Usage: endec [OPTION]"

getAction :: Flag -> [Flag] -> IO ()
getAction Help _                 = usageAction
getAction Encoding xs            = doEnc (getFilename xs)
getAction Decoding xs            = doDec (getFilename xs)
getAction (Filename path) (x:xs) = getAction x ((Filename path):xs)

getFilename :: [Flag] -> FilePath
getFilename [] = defaultFilename
getFilename (o:os) = isFilename o
  where
    isFilename (Filename s) = s
    isFilename _  = getFilename os

-- vim: set ts=2 sts=2 sw=2 ai et tw=78:
