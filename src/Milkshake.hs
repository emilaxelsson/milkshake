-- | Utilities for building static sites using Shake
--
-- Note that operations such as 'doesDirectoryExist' and 'getDirectoryFiles'
-- from "Development.Shake" will track the results as dependencies, and thus
-- should only be used on source files. The same does not apply to the `Action`
-- functions defined in this module.
--
-- See the @examples/@ directory for how to use Milkshake.

module Milkshake where



import Control.Monad
import qualified Data.HashSet as HashSet
import Development.Shake
import Development.Shake.FilePath
import qualified System.Directory as Sys
import qualified System.Directory.Extra as Sys



-- | Like 'Sys.listContents', but only returns the directories in a directory
listDirs :: FilePath -> IO [FilePath]
listDirs dir = filterM Sys.doesDirectoryExist =<< Sys.listContents dir

-- | Top-down, breadth-first traversal of a directory structure
--
-- The 'FilePath' given to the step function is rooted in the root directory.
-- The working directory is not affected.
--
-- The results of sub-trees are combined using 'mappend'.
topDownDir :: Monoid a
    => (FilePath -> Action a)  -- ^ Function to perform at each step
    -> FilePath                -- ^ Root directory
    -> Action a
topDownDir step = go
  where
    go dir = do
      a    <- step dir
      aSub <- fmap mconcat $ mapM go =<< liftIO (listDirs dir)
      return (a `mappend` aSub)

-- | Top-down, breadth-first traversal of a directory structure
--
-- The 'FilePath' given to the step function is rooted in the root directory.
-- The working directory is not affected.
topDownDir_
    :: (FilePath -> Action ())  -- ^ Function to perform at each step
    -> FilePath                 -- ^ Root directory
    -> Action ()
topDownDir_ step = go
  where
    go dir = do
      step dir
      mapM_ go =<< liftIO (listDirs dir)

-- | Bottom-up traversal of a directory structure
--
-- The 'FilePath' given to the step function is rooted in the root directory.
-- The working directory is not affected.
--
-- The results of sub-trees are combined using 'mappend'.
bottomUpDir :: Monoid a
    => (FilePath -> Action a)  -- ^ Function to perform at each step
    -> FilePath                -- ^ Root directory
    -> Action a
bottomUpDir step = go
  where
    go dir = do
      aSub <- fmap mconcat $ mapM go =<< liftIO (listDirs dir)
      a    <- step dir
      return (aSub `mappend` a)

-- | Bottom-up traversal of a directory structure
--
-- The 'FilePath' given to the step function is rooted in the root directory.
-- The working directory is not affected.
bottomUpDir_
    :: (FilePath -> Action ())  -- ^ Function to perform at each step
    -> FilePath                 -- ^ Root directory
    -> Action ()
bottomUpDir_ step = go
  where
    go dir = do
      mapM_ go =<< liftIO (listDirs dir)
      step dir

-- | Forcibly remove a file or directory
removePath :: FilePath -> Action ()
removePath path = traced ("rm -rf " ++ path) $ Sys.removePathForcibly path
  -- More portable than `rm -rf`

-- | Create directory if missing
createDirIfMissing :: FilePath -> Action ()
createDirIfMissing dir = do
    exists <- doesDirectoryExist dir
    unless exists $ traced ("mkdir " ++ dir) $ Sys.createDirectory dir
  -- More portable than `mkdir`
  --
  -- Using `liftIO . Sys.createDirectoryIfMissing` would create an `Action` that
  -- always runs.

-- | Remove files in the root directory that are not in the set of expected
-- files
--
-- Sub-directories that don't contain any expected files are also removed. The
-- traversal starts from the top, so in case of an unneeded directory with
-- unneeded files inside, the directory and all its contents will be removed in
-- a single operation.
removeLegacy
    :: [FilePath]
         -- ^ Expected set of files (not directories), rooted in the root
         -- directory
    -> FilePath  -- ^ Root directory
    -> Action ()
removeLegacy expectedFiles = topDownDir_ $ \dir -> do
    -- Remove legacy directories
    ds <- HashSet.fromList <$> liftIO (listDirs dir)
    let legacyDirs = HashSet.difference ds dirs
    mapM_ removePath legacyDirs
    -- Remove legacy files
    fs <- HashSet.fromList <$> liftIO (Sys.listFiles dir)
    let legacyFiles = HashSet.difference fs files
    mapM_ removePath legacyFiles
  where
    files = HashSet.fromList expectedFiles
    dirs  = HashSet.map takeDirectory files

