-- Suppose we have a directory structure such as the following:
--
-- > siteSrc
-- >  ├╴style.css
-- >  ├╴page1.md
-- >  └╴dir
-- >     ├╴page2.md
-- >     └╴page3.md
--
-- From this source we want to generate the following static web site:
--
-- > siteDst
-- >  ├╴style.css
-- >  ├╴page1.html
-- >  └╴dir
-- >     ├╴page2.html
-- >     └╴page3.html
--
-- Regardless of directory structure, we want to use the following rules to
-- generate files:
--
-- * Use Pandoc to convert from Markdown to HTML
--
-- * Copy CSS files as they are
--
-- Using "Development.Shake" and "Milkshake", we can express this site generator
-- as the following Shake script.

{-# OPTIONS_GHC -Wno-missing-signatures #-}

import Development.Shake
import Development.Shake.FilePath
import Milkshake

siteSrc = "examples" </> "siteSrc"
siteDst = "examples" </> "siteDst"

-- | Replace two top-level directory of a path
setTop
    :: FilePath  -- ^ New top
    -> FilePath  -- ^ Old path
    -> FilePath
setTop top = (top </>) . dropDirectory1 . dropDirectory1

-- | All files that should be generated
siteFiles :: Action [FilePath]
siteFiles = do
    htmls <- map (-<.> "html") <$> getDirectoryFiles siteSrc ["//*.md"]
    csss  <- getDirectoryFiles siteSrc ["//*.css"]
    return $ map (siteDst </>) $
      htmls ++ csss

main = shakeArgs shakeOptions $ do

    want ["site"]

    "site" ~> do
      createDirIfMissing siteDst
      need =<< siteFiles
      need ["remove-legacy"]

    -- How to build HTML files
    (siteDst <//> "*.html") %> \out -> do
      let inp = setTop siteSrc out -<.> "md"
      need [inp]
      unit $ cmd "pandoc -s" inp "-o" out

    -- How to copy CSS files
    (siteDst <//> "*.css") %> \out -> do
      let inp = setTop siteSrc out
      copyFileChanged inp out

    "remove-legacy" ~> (flip removeLegacy siteDst =<< siteFiles)
      -- Remove files and directories in `siteDst` that are not found in
      -- `siteFiles`. Note that there is no need to also check that the expected
      -- files are present, since this is automatically checked for rules
      -- expressed using `%>`.

