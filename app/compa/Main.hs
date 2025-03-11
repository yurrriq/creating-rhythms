{-# LANGUAGE TypeApplications #-}

import Data.List (sort)
import Data.Rhythm.Compositions (compositionsAllowed)
import Options.Applicative

main :: IO ()
main =
  putStr
    . unlines
    . sort
    . map (unwords . map show)
    . uncurry (flip (compositionsAllowed @Integer))
    =<< customExecParser p opts
  where
    opts =
      info
        ( (,)
            <$> argument auto (metavar "N")
            <*> some (argument auto (metavar "P1 P2 ..."))
        )
        (fullDesc <> progDesc "All positive compositions with allowed parts.")
    p = prefs showHelpOnEmpty
