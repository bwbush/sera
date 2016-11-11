module Main (
  main
) where


import Control.Arrow ((***), first, second)
import Control.Monad (when)
import System.Environment (getArgs)

import qualified Data.ByteString.Char8 as BS

main :: IO ()
main =
  do
    args <- getArgs
    case args of
      []    -> putStrLn "Usage: unmangle-excel files..."
      paths -> mapM_ unmangle paths


unmangle :: FilePath -> IO ()
unmangle path =
  do
    (modified, xs) <- unmangle' <$> BS.readFile path
    when modified
      $ BS.writeFile path xs


unmangle' :: BS.ByteString -> (Bool, BS.ByteString)
unmangle' xs
  | BS.null xs         = (False, BS.empty)
  | BS.head xs == '"'  = first (const True)            . unmangle' $ BS.tail xs
  | BS.head xs == '\r' = const True *** BS.cons '\n'   $ unmangle' $ BS.tail xs
  | otherwise          = second (BS.cons $ BS.head xs) . unmangle' $ BS.tail xs
