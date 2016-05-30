module Paths_key (
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
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/samuel/.cabal/bin"
libdir     = "/home/samuel/.cabal/lib/x86_64-linux-ghc-7.10.3/key-0.1.0.0-DTPvsLqBDh1Ld6ERmRFZpH"
datadir    = "/home/samuel/.cabal/share/x86_64-linux-ghc-7.10.3/key-0.1.0.0"
libexecdir = "/home/samuel/.cabal/libexec"
sysconfdir = "/home/samuel/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "key_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "key_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "key_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "key_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "key_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
