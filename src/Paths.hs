module Paths (getDataFile) where

import System.FilePath
import System.IO.Unsafe
import System.Environment.Executable
import System.Info
import Paths_Animations (getDataDir)


getDataDir :: IO FilePath
getDataDir = return "../data/"

getDataFile :: FilePath -> FilePath
getDataFile x = unsafePerformIO $ fmap (</> x) Paths.getDataDir
