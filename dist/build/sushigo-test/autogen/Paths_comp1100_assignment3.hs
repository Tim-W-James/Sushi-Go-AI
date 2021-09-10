{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_comp1100_assignment3 (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\timja\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\timja\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.2.2\\comp1100-assignment3-0.1.0.0-6N3rSIy1knV7MUuYTqE83T-sushigo-test"
dynlibdir  = "C:\\Users\\timja\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.2.2"
datadir    = "C:\\Users\\timja\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.2.2\\comp1100-assignment3-0.1.0.0"
libexecdir = "C:\\Users\\timja\\AppData\\Roaming\\cabal\\comp1100-assignment3-0.1.0.0-6N3rSIy1knV7MUuYTqE83T-sushigo-test\\x86_64-windows-ghc-8.2.2\\comp1100-assignment3-0.1.0.0"
sysconfdir = "C:\\Users\\timja\\AppData\\Roaming\\cabal\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "comp1100_assignment3_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "comp1100_assignment3_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "comp1100_assignment3_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "comp1100_assignment3_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "comp1100_assignment3_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "comp1100_assignment3_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
