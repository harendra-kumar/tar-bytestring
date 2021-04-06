{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
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
  unpack',
  ) where

import Codec.Archive.Tar.Types
import Codec.Archive.Tar.Check

import Data.List (partition, nub)
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as BS
import System.FilePath
         ( (</>) )
import qualified System.FilePath as FilePath.Native
         ( takeDirectory )
import System.Directory
         ( createDirectoryIfMissing, copyFile, getDirectoryContents, doesDirectoryExist )
import Control.Exception
         ( Exception, throwIO, handle )
import System.IO.Error
         ( ioeGetErrorType )
import GHC.IO (unsafeInterleaveIO)
import Data.Foldable (traverse_)
import GHC.IO.Exception (IOErrorType(InappropriateType, IllegalOperation, PermissionDenied, InvalidArgument))
#if MIN_VERSION_directory(1,2,3)
import System.Directory
         ( setModificationTime )
import Data.Time.Clock.POSIX
         ( posixSecondsToUTCTime )
import Control.Exception as Exception
         ( catch )
import System.IO.Error
         ( isPermissionError )
#endif
#if MIN_VERSION_directory(1,3,1)
import System.Directory
         ( createDirectoryLink, createFileLink )
#endif
import qualified Streamly.Internal.FileSystem.File as File
import Streamly
import Streamly.Prelude (cons, consM)
import Control.Monad.Trans.Except
import Control.Monad.IO.Class (liftIO)

unpack' :: FilePath -> SerialT (ExceptT FormatError IO) Entry -> SerialT (ExceptT FormatError IO) ()
unpack' baseDir stream = do
  stream >>= \e -> unpackEntry e

  where
    unpackEntry entry@(Entry { entryContent = (NormalFileS file _)}) =
        liftIO $ extractFile' (entryPath entry) file (entryTime entry)
    unpackEntry entry@(Entry { entryContent = Directory}) =
        liftIO $ extractDir (entryPath entry) (entryTime entry)
    -- unpackEntry entry@(Entry { entryContent = (HardLink link)}) =
        -- (unpackEntries $! saveLink True (entryPath entry) link links) es
    -- unpackEntry entry@(Entry { entryContent = (SymbolicLink link)}) =
        -- (unpackEntries $! saveLink False (entryPath entry) link links) es
    unpackEntry _ = pure ()

    extractFile' path content mtime = do
      createDirectoryIfMissing True absDir
      -- File.fromBytes absPath content
      -- setModTime absPath mtime
      where
        absDir  = baseDir </> FilePath.Native.takeDirectory path
        absPath = baseDir </> path

    extractDir path mtime = do
      createDirectoryIfMissing True absPath
      setModTime absPath mtime
      where
        absPath = baseDir </> path

unpack :: Exception e => FilePath -> Entries e -> IO ()
unpack baseDir entries = do
  uEntries <- unpackEntries [] (checkSecurity entries)
  let (hardlinks, symlinks) = partition (\(_, _, x) -> x) uEntries
  -- handle hardlinks first, in case a symlink points to it
  handleHardLinks hardlinks
  handleSymlinks symlinks

  where
    -- We're relying here on 'checkSecurity' to make sure we're not scribbling
    -- files all over the place.

    unpackEntries _     (Fail err)      = either throwIO throwIO err
    unpackEntries links Done            = return links
    unpackEntries links (Next entry es) = case entryContent entry of
      NormalFile file _ -> do
        extractFile (entryPath entry) file (entryTime entry)
        unpackEntries links es
      NormalFileS file _ -> do
        extractFile' (entryPath entry) file (entryTime entry)
        unpackEntries links es
      Directory         -> extractDir (entryPath entry) (entryTime entry)
                        >> unpackEntries links es
      HardLink     link -> (unpackEntries $! saveLink True (entryPath entry) link links) es
      SymbolicLink link -> (unpackEntries $! saveLink False (entryPath entry) link links) es
      OtherEntryType 'L' (Char8.unpack . BS.toStrict -> fn) _ ->
        case es of
             (Next entry' es') -> case entryContent entry' of
               NormalFile file _ -> do
                extractFile fn file (entryTime entry')
                unpackEntries links es'
               Directory         -> extractDir fn (entryTime entry')
                                 >> unpackEntries links es'
               HardLink     link -> (unpackEntries $! saveLink True fn link links) es'
               SymbolicLink link -> (unpackEntries $! saveLink False fn link links) es'
               OtherEntryType 'L' _ _ -> throwIO $ userError "Two subsequent OtherEntryType 'L'"
               _ -> unpackEntries links es'
             (Fail err)      -> either throwIO throwIO err
             Done            -> throwIO $ userError "././@LongLink without a subsequent entry"
      _ -> unpackEntries links es --ignore other file types

    extractFile path content mtime = do
      -- Note that tar archives do not make sure each directory is created
      -- before files they contain, indeed we may have to create several
      -- levels of directory.
      -- if (path == "ghc-8.4.1/compiler/stage2/build/libHSghc-8.4.1_p.a"
        -- || path == "ghc-8.4.1/compiler/stage2/build/libHSghc-8.4.1.a"
        -- || path == "ghc-8.4.1/libraries/Cabal/Cabal/dist-install/build/libHSCabal-2.2.0.0_p.a"
        -- )
      -- then pure ()
      -- else do
      createDirectoryIfMissing True absDir
      BS.writeFile absPath content
      setModTime absPath mtime
      where
        absDir  = baseDir </> FilePath.Native.takeDirectory path
        absPath = baseDir </> path

    extractFile' path content mtime = do
      createDirectoryIfMissing True absDir
      File.fromBytes absPath content
      setModTime absPath mtime
      where
        absDir  = baseDir </> FilePath.Native.takeDirectory path
        absPath = baseDir </> path

    extractDir path mtime = do
      createDirectoryIfMissing True absPath
      setModTime absPath mtime
      where
        absPath = baseDir </> path

    saveLink isHardLink path link links = seq (length path)
                                        $ seq (length link')
                                        $ (path, link', isHardLink):links
      where link' = fromLinkTarget link


    -- for hardlinks, we just copy
    handleHardLinks = mapM_ $ \(relPath, relLinkTarget, _) ->
      let absPath   = baseDir </> relPath
          -- hard links link targets are always "absolute" paths in
          -- the context of the tar root
          absTarget = baseDir </> relLinkTarget
      -- we don't expect races here, since we should be the
      -- only process unpacking the tar archive and writing to
      -- the destination
      in doesDirectoryExist absTarget >>= \case
          True -> copyDirectoryRecursive absTarget absPath
          False -> copyFile absTarget absPath

    -- For symlinks, we first try to recreate them and if that fails
    -- with 'IllegalOperation', 'PermissionDenied' or 'InvalidArgument',
    -- we fall back to copying.
    -- This error handling isn't too fine grained and maybe should be
    -- platform specific, but this way it might catch erros on unix even on
    -- FAT32 fuse mounted volumes.
    handleSymlinks = mapM_ $ \(relPath, relLinkTarget, _) ->
      let absPath   = baseDir </> relPath
          -- hard links link targets are always "absolute" paths in
          -- the context of the tar root
          absTarget = FilePath.Native.takeDirectory absPath </> relLinkTarget
      -- we don't expect races here, since we should be the
      -- only process unpacking the tar archive and writing to
      -- the destination
      in doesDirectoryExist absTarget >>= \case
#if MIN_VERSION_directory(1,3,1)
          True -> handleSymlinkError (copyDirectoryRecursive absTarget absPath)
            $ createDirectoryLink relLinkTarget absPath
          False -> handleSymlinkError (copyFile absTarget absPath)
            $ createFileLink relLinkTarget absPath

      where
        handleSymlinkError action =
          handle (\e -> if ioeGetErrorType e `elem` [IllegalOperation
                                                    ,PermissionDenied
                                                    ,InvalidArgument]
                      then action
                      else throwIO e
                 )
#else
          True -> copyDirectoryRecursive absTarget absPath
          False -> copyFile absTarget absPath
#endif

-- | Recursively copy the contents of one directory to another path.
--
-- This is a rip-off of Cabal library.
copyDirectoryRecursive :: FilePath -> FilePath -> IO ()
copyDirectoryRecursive srcDir destDir = do
  srcFiles <- getDirectoryContentsRecursive srcDir
  copyFilesWith copyFile destDir [ (srcDir, f)
                                   | f <- srcFiles ]
  where
    -- | Common implementation of 'copyFiles', 'installOrdinaryFiles',
    -- 'installExecutableFiles' and 'installMaybeExecutableFiles'.
    copyFilesWith :: (FilePath -> FilePath -> IO ())
                  -> FilePath -> [(FilePath, FilePath)] -> IO ()
    copyFilesWith doCopy targetDir srcFiles = do

      -- Create parent directories for everything
      let dirs = map (targetDir </>) . nub . map (FilePath.Native.takeDirectory . snd) $ srcFiles
      traverse_ (createDirectoryIfMissing True) dirs

      -- Copy all the files
      sequence_ [ let src  = srcBase   </> srcFile
                      dest = targetDir </> srcFile
                   in doCopy src dest
                | (srcBase, srcFile) <- srcFiles ]

    -- | List all the files in a directory and all subdirectories.
    --
    -- The order places files in sub-directories after all the files in their
    -- parent directories. The list is generated lazily so is not well defined if
    -- the source directory structure changes before the list is used.
    --
    getDirectoryContentsRecursive :: FilePath -> IO [FilePath]
    getDirectoryContentsRecursive topdir = recurseDirectories [""]
      where
        recurseDirectories :: [FilePath] -> IO [FilePath]
        recurseDirectories []         = return []
        recurseDirectories (dir:dirs) = unsafeInterleaveIO $ do
          (files, dirs') <- collect [] [] =<< getDirectoryContents (topdir </> dir)
          files' <- recurseDirectories (dirs' ++ dirs)
          return (files ++ files')

          where
            collect files dirs' []              = return (reverse files
                                                         ,reverse dirs')
            collect files dirs' (entry:entries) | ignore entry
                                                = collect files dirs' entries
            collect files dirs' (entry:entries) = do
              let dirEntry = dir </> entry
              isDirectory <- doesDirectoryExist (topdir </> dirEntry)
              if isDirectory
                then collect files (dirEntry:dirs') entries
                else collect (dirEntry:files) dirs' entries

            ignore ['.']      = True
            ignore ['.', '.'] = True
            ignore _          = False

setModTime :: FilePath -> EpochTime -> IO ()
#if MIN_VERSION_directory(1,2,3)
-- functionality only supported as of directory-1.2.3.x
setModTime path t =
    setModificationTime path (posixSecondsToUTCTime (fromIntegral t))
      `Exception.catch` \e ->
        if isPermissionError e then return () else throwIO e
#else
setModTime _path _t = return ()
#endif
