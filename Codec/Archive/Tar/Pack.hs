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
module Codec.Archive.Tar.Pack (
    pack,
    packFileEntry,
    packDirectoryEntry,

    getDirectoryContentsRecursive,
  ) where

import Codec.Archive.Tar.Types
import Control.Applicative
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as L
import qualified System.Posix.IO.ByteString as SPI
import System.Posix.FD
import System.Posix.ByteString.FilePath (RawFilePath)
import qualified System.Posix.FilePath as FilePath.Posix
import System.Posix.FilePath ( (</>), isSpecialDirectoryEntry )
import HPath (toFilePath)
import HPath.IO
import Data.Time.Clock
         ( UTCTime )
import Data.Time.Clock.POSIX
         ( utcTimeToPOSIXSeconds )
import System.IO
         ( IOMode(ReadMode), openBinaryFile, hFileSize )
import System.IO.Unsafe (unsafeInterleaveIO)

-- | Creates a tar archive from a list of directory or files. Any directories
-- specified will have their contents included recursively. Paths in the
-- archive will be relative to the given base directory.
--
-- This is a portable implementation of packing suitable for portable archives.
-- In particular it only constructs 'NormalFile' and 'Directory' entries. Hard
-- links and symbolic links are treated like ordinary files. It cannot be used
-- to pack directories containing recursive symbolic links. Special files like
-- FIFOs (named pipes), sockets or device files will also cause problems.
--
-- An exception will be thrown for any file names that are too long to
-- represent as a 'TarPath'.
--
-- * This function returns results lazily. Subdirectories are scanned
-- and files are read one by one as the list of entries is consumed.
--
pack :: RawFilePath   -- ^ Base directory
     -> [RawFilePath] -- ^ Files and directories to pack, relative to the base dir
     -> IO [Entry]
pack baseDir paths0 = preparePaths baseDir paths0 >>= packPaths baseDir

preparePaths :: RawFilePath -> [RawFilePath] -> IO [RawFilePath]
preparePaths baseDir paths =
  fmap concat $ interleave
    [ do isDir  <- withRawFilePath (baseDir </> path) $ \p -> either doesDirectoryExist doesDirectoryExist p
         if isDir
           then do entries <- getDirectoryContentsRecursive (baseDir </> path)
                   let entries' = map (path </>) entries
                       dir = FilePath.Posix.addTrailingPathSeparator path
                   if BS.null path then return entries'
                                else return (dir : entries')
           else return [path]
    | path <- paths ]

packPaths :: RawFilePath -> [RawFilePath] -> IO [Entry]
packPaths baseDir paths =
  interleave
    [ do tarpath <- either fail return (toTarPath isDir relpath)
         if isDir then packDirectoryEntry filepath tarpath
                  else packFileEntry      filepath tarpath
    | relpath <- paths
    , let isDir    = FilePath.Posix.hasTrailingPathSeparator filepath
          filepath = baseDir </> relpath ]

interleave :: [IO a] -> IO [a]
interleave = unsafeInterleaveIO . go
  where
    go []     = return []
    go (x:xs) = do
      x'  <- x
      xs' <- interleave xs
      return (x':xs')

-- | Construct a tar 'Entry' based on a local file.
--
-- This sets the entry size, the data contained in the file and the file's
-- modification time. If the file is executable then that information is also
-- preserved. File ownership and detailed permissions are not preserved.
--
-- * The file contents is read lazily.
--
packFileEntry :: RawFilePath -- ^ Full path to find the file on the local disk
              -> TarPath  -- ^ Path to use for the tar Entry in the archive
              -> IO Entry
packFileEntry filepath tarpath = do
  mtime   <- getModTime filepath
  executable   <- withRawFilePath filepath $ either isExecutable isExecutable
  file    <- openFd filepath SPI.ReadOnly [] Nothing >>= SPI.fdToHandle
  size    <- hFileSize file
  content <- L.hGetContents file
  return (simpleEntry tarpath (NormalFile content (fromIntegral size))) {
    entryPermissions = if executable then executableFilePermissions
                                     else ordinaryFilePermissions,
    entryTime = mtime
  }

-- | Construct a tar 'Entry' based on a local directory (but not its contents).
--
-- The only attribute of the directory that is used is its modification time.
-- Directory ownership and detailed permissions are not preserved.
--
packDirectoryEntry :: RawFilePath -- ^ Full path to find the file on the local disk
                   -> TarPath  -- ^ Path to use for the tar Entry in the archive
                   -> IO Entry
packDirectoryEntry filepath tarpath = do
  mtime   <- getModTime filepath
  return (directoryEntry tarpath) {
    entryTime = mtime
  }

-- | This is a utility function, much like 'getDirectoryContents'. The
-- difference is that it includes the contents of subdirectories.
--
-- The paths returned are all relative to the top directory. Directory paths
-- are distinguishable by having a trailing path separator
-- (see 'FilePath.Posix.hasTrailingPathSeparator').
--
-- All directories are listed before the files that they contain. Amongst the
-- contents of a directory, subdirectories are listed after normal files. The
-- overall result is that files within a directory will be together in a single
-- contiguous group. This tends to improve file layout and IO performance when
-- creating or extracting tar archives.
--
-- * This function returns results lazily. Subdirectories are not scanned
-- until the files entries in the parent directory have been consumed.
--
getDirectoryContentsRecursive :: RawFilePath -> IO [RawFilePath]
getDirectoryContentsRecursive dir0 =
  fmap tail (recurseDirectories dir0 [BS.empty])

recurseDirectories :: RawFilePath -> [RawFilePath] -> IO [RawFilePath]
recurseDirectories _    []         = return []
recurseDirectories base (dir:dirs) = unsafeInterleaveIO $ do
  (files, dirs') <- collect [] [] =<< ((fmap . fmap) toFilePath $ (withRawFilePath (base </> dir) $ either getDirsFiles' getDirsFiles'))

  files' <- recurseDirectories base (dirs' ++ dirs)
  return (dir : files ++ files')

  where
    collect files dirs' []              = return (reverse files, reverse dirs')
    collect files dirs' (entry:entries) | isSpecialDirectoryEntry entry
                                        = collect files dirs' entries
    collect files dirs' (entry:entries) = do
      let dirEntry  = dir </> entry
          dirEntry' = FilePath.Posix.addTrailingPathSeparator dirEntry
      isDirectory <- withRawFilePath (base </> dirEntry) $ either doesDirectoryExist doesDirectoryExist
      if isDirectory
        then collect files (dirEntry':dirs') entries
        else collect (dirEntry:files) dirs' entries


getModTime :: RawFilePath -> IO EpochTime
getModTime path = do
  t <- withRawFilePath path $ either getModificationTime getModificationTime
  return . floor . utcTimeToPOSIXSeconds $ t
