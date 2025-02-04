{-# LANGUAGE TypeApplications #-}

import Data.Rhythm.Binary (intervalsToBinary)

main :: IO ()
main =
  putStrLn . intervalsToBinary . map (read @Int) . words =<< getLine
