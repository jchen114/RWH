module Paths_CH21 (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/Kingpin/Dropbox/HaskellDoodle/RWH/CH21/.cabal-sandbox/bin"
libdir     = "/Users/Kingpin/Dropbox/HaskellDoodle/RWH/CH21/.cabal-sandbox/lib/x86_64-osx-ghc-7.8.3/CH21-0.1.0.0"
datadir    = "/Users/Kingpin/Dropbox/HaskellDoodle/RWH/CH21/.cabal-sandbox/share/x86_64-osx-ghc-7.8.3/CH21-0.1.0.0"
libexecdir = "/Users/Kingpin/Dropbox/HaskellDoodle/RWH/CH21/.cabal-sandbox/libexec"
sysconfdir = "/Users/Kingpin/Dropbox/HaskellDoodle/RWH/CH21/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "CH21_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "CH21_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "CH21_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "CH21_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "CH21_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
