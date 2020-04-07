{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

import Data.List (partition)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as L
import System.Posix.FilePath
         ( (</>), normalise )
import qualified System.Posix.FilePath as FilePath.Native
         ( takeDirectory )
import           Control.Exception.Safe              ( Exception
                                                     , throwIO
                                                     , finally
                                                     )
import Data.Time.Clock.POSIX
         ( posixSecondsToUTCTime )
import Control.Exception.Safe as Exception
         ( catch, handle, throw )
import Control.Monad (when)
import System.IO.Error
         ( isPermissionError )
import System.Posix.RawFilePath.Directory hiding (Directory, SymbolicLink)
import System.Posix.RawFilePath.Directory.Errors
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
unpack baseDir entries = do
  uEntries <- unpackEntries [] (checkSecurity entries)
  let (hardlinks, symlinks) = partition (\(_, _, x) -> x) uEntries
  -- emulate hardlinks first, in case a symlink points to it
  emulateLinks hardlinks
  emulateLinks symlinks

  where
    -- We're relying here on 'checkSecurity' to make sure we're not scribbling
    -- files all over the place.

    unpackEntries _     (Fail err)      = either throwIO throwIO err
    unpackEntries links Done            = return links
    unpackEntries links (Next entry es) = case entryContent entry of
      NormalFile file _ -> do
        extractFile (entryPermissions entry) (entryPath entry) file (entryTime entry)
        unpackEntries links es
      Directory         -> extractDir (entryPath entry) (entryTime entry)
                        >> unpackEntries links es
      HardLink     link -> (unpackEntries $! saveLink True (entryPath entry) link links) es
      SymbolicLink link -> (unpackEntries $! saveLink False (entryPath entry) link links) es
      OtherEntryType 'L' fn _ ->
        case es of
             (Next entry' es') -> case entryContent entry' of
               NormalFile file _ -> do
                extractFile (entryPermissions entry') (L.toStrict fn) file (entryTime entry')
                unpackEntries links es'
               Directory         -> extractDir (L.toStrict fn) (entryTime entry')
                                 >> unpackEntries links es'
               HardLink     link -> (unpackEntries $! saveLink True (L.toStrict fn) link links) es'
               SymbolicLink link -> (unpackEntries $! saveLink False (L.toStrict fn) link links) es'
               OtherEntryType 'L' _ _ -> throwIO $ userError "Two subsequent OtherEntryType 'L'"
               _ -> unpackEntries links es'
             (Fail err)      -> either throwIO throwIO err
             Done            -> throwIO $ userError "././@LongLink without a subsequent entry"
      _ -> unpackEntries links es --ignore other file types

    extractFile fPerms path content mtime = do
      -- Note that tar archives do not make sure each directory is created
      -- before files they contain, indeed we may have to create several
      -- levels of directory.
      createDirRecursive dirPerms (normalise absDir)
      writeFileL absPath (Just fPerms) content
      setModTime absPath mtime
      where
        absDir  = baseDir </> FilePath.Native.takeDirectory path
        absPath = baseDir </> path

    extractDir path mtime = do
      createDirRecursive dirPerms (normalise absPath)
      setModTime absPath mtime
      where
        absPath = baseDir </> path

    saveLink isHardLink path link links = seq (BS.length path)
                                        $ seq (BS.length link')
                                        $ (path, link', isHardLink):links
      where link' = fromLinkTarget link

    emulateLinks = mapM_ $ \(relPath, relLinkTarget, isHardLink) ->
      let absPath   = baseDir </> relPath
          -- hard links link targets are always "absolute" paths in
          -- the context of the tar root
          absTarget = if isHardLink then baseDir </> relLinkTarget else FilePath.Native.takeDirectory absPath </> relLinkTarget
      -- some archives have broken recursive links
      in handle (\(ex :: HPathIOException) ->
                 when (not . isSameFile $ ex) $ throw ex)
           $ copyFile absTarget absPath Overwrite


setModTime :: RawFilePath -> EpochTime -> IO ()
setModTime p t = setModificationTime p (fromIntegral t)
            `Exception.catch` \e ->
              if isPermissionError e then return () else throwIO e
