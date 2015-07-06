module Paths_DCFL (
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
version = Version [0,1,3,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/dhaivat/dev/dcfl/.cabal-sandbox/bin"
libdir     = "/Users/dhaivat/dev/dcfl/.cabal-sandbox/lib/x86_64-osx-ghc-7.6.3/DCFL-0.1.3.0"
datadir    = "/Users/dhaivat/dev/dcfl/.cabal-sandbox/share/x86_64-osx-ghc-7.6.3/DCFL-0.1.3.0"
libexecdir = "/Users/dhaivat/dev/dcfl/.cabal-sandbox/libexec"
sysconfdir = "/Users/dhaivat/dev/dcfl/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "DCFL_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "DCFL_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "DCFL_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "DCFL_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "DCFL_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
