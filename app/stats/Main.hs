{-# LANGUAGE LambdaCase #-}

-- | Tools for task statistics files.
module Main (main) where

import Hyperion.OsString   (encodeUtf)
import Hyperion.Scheduler  (mergeStatFiles)
import Options.Applicative

data Command = Merge FilePath [FilePath]

main :: IO ()
main = execParser (info (commands <**> helper) (progDesc "Tools for task statistics files")) >>= \case
  Merge out statFiles -> do
    out' <- encodeUtf out
    statFiles' <- traverse encodeUtf statFiles
    mergeStatFiles out' statFiles'
  where
    commands = hsubparser $
      command "merge" (info merge (progDesc "Merge statistics files into one"))
    merge = Merge
      <$> strOption (short 'o' <> long "output" <> metavar "FILE" <> help "The merged file")
      <*> some (strArgument (metavar "FILE..." <> help "Statistics files to merge"))
