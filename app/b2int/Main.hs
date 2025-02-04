import Data.Rhythm.Binary (binaryToIntervals)

main :: IO ()
main =
  putStrLn . unwords . map show . binaryToIntervals =<< getLine
