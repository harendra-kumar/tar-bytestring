{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Codec.Archive.Tar
-- Copyright   :  (c) 2007 Bjorn Bringert,
--                    2008 Andrea Vezzosi,
--                    2008-2009, 2012, 2016 Duncan Coutts
-- License     :  BSD3
--
-- Maintainer  :  duncan@community.haskell.org
-- Portability :  portable
--
-----------------------------------------------------------------------------
module Codec.Archive.Tar.Unpack (
  unpack,
  ) where

import Codec.Archive.Tar.Types
import Codec.Archive.Tar.Check

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as L
import System.Posix.FilePath
         ( (</>) )
import qualified System.Posix.FilePath as FilePath.Native
         ( takeDirectory )
import           Control.Exception              ( Exception
                                                , throwIO
                                                , finally
                                                )
import Data.Time.Clock.POSIX
         ( posixSecondsToUTCTime )
import Control.Exception as Exception
         ( catch )
import System.IO.Error
         ( isPermissionError )
import System.Posix.RawFilePath.Directory hiding (Directory, SymbolicLink)
import qualified System.Posix.IO.ByteString as SPI
import qualified System.Posix as Posix
import System.Posix.FD
import System.IO (hClose)

-- | Create local files and directories based on the entries of a tar archive.
--
-- This is a portable implementation of unpacking suitable for portable
-- archives. It handles 'NormalFile' and 'Directory' entries and has simulated
-- support for 'SymbolicLink' and 'HardLink' entries. Links are implemented by
-- copying the target file. This therefore works on Windows as well as Unix.
-- All other entry types are ignored, that is they are not unpacked and no
-- exception is raised.
--
-- If the 'Entries' ends in an error then it is raised an an exception. Any
-- files or directories that have been unpacked before the error was
-- encountered will not be deleted. For this reason you may want to unpack
-- into an empty directory so that you can easily clean up if unpacking fails
-- part-way.
--
-- On its own, this function only checks for security (using 'checkSecurity').
-- You can do other checks by applying checking functions to the 'Entries' that
-- you pass to this function. For example:
--
-- > unpack dir (checkTarbomb expectedDir entries)
--
-- If you care about the priority of the reported errors then you may want to
-- use 'checkSecurity' before 'checkTarbomb' or other checks.
--
unpack :: Exception e => RawFilePath -> Entries e -> IO ()
unpack baseDir entries = unpackEntries [] (checkSecurity entries)
                     >>= emulateLinks

  where
    -- We're relying here on 'checkSecurity' to make sure we're not scribbling
    -- files all over the place.

    unpackEntries _     (Fail err)      = either throwIO throwIO err
    unpackEntries links Done            = return links
    unpackEntries links (Next entry es) = case entryContent entry of
      NormalFile file _ -> extractFile path file mtime
                        >> unpackEntries links es
      Directory         -> extractDir path mtime
                        >> unpackEntries links es
      HardLink     link -> (unpackEntries $! saveLink path link links) es
      SymbolicLink link -> (unpackEntries $! saveLink path link links) es
      _                 -> unpackEntries links es --ignore other file types
      where
        path  = entryPath entry
        mtime = entryTime entry

    extractFile path content mtime = do
      -- Note that tar archives do not make sure each directory is created
      -- before files they contain, indeed we may have to create several
      -- levels of directory.
      createDirRecursive newDirPerms absDir
      writeFileL absPath (Just newFilePerms) content
      setModTime absPath mtime
      where
        absDir  = baseDir </> FilePath.Native.takeDirectory path
        absPath = baseDir </> path

    extractDir path mtime = do
      createDirRecursive newDirPerms absPath
      setModTime absPath mtime
      where
        absPath = baseDir </> path

    saveLink path link links = seq (BS.length path)
                             $ seq (BS.length link')
                             $ (path, link'):links
      where link' = fromLinkTarget link

    emulateLinks = mapM_ $ \(relPath, relLinkTarget) -> do
      let absPath = baseDir </> relPath
          absTarget = FilePath.Native.takeDirectory absPath </> relLinkTarget
      copyFile absTarget absPath Overwrite

setModTime :: RawFilePath -> EpochTime -> IO ()
setModTime p t = setModificationTime p (fromIntegral t)
            `Exception.catch` \e ->
              if isPermissionError e then return () else throwIO e
