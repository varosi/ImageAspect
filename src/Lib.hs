{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lib where

import           Codec.Picture
import           Codec.Picture.Metadata
import           Control.Exception      (IOException, handle)
import           Control.Monad          (forM_)
import           Data.Ratio
import           Data.Text              (breakOnEnd, pack, unpack)
import           Path
import           Path.IO                (renameFile)
import           Prelude                hiding (lookup)
import           System.Directory       hiding (renameFile)

extensions :: [String]
extensions = [".jpg",".png", ".tif", ".tiff", ".gif", ".tga", ".bmp", ".hdr", ".pic"]

allImages :: IO [Path Rel File]
allImages = do
    files <- listDirectory "."
    files' <- mapM parseRelFile files
    pure . filter (\p -> fileExtension p `elem` extensions) $ files'

closestRatio :: Ratio Word -> Ratio Word
closestRatio = closer where
    ratios = [ a % b | a <- [1..30], b <- [1..30] ]
    sm z   = filter (<= z) ratios
    lg z   = filter (> z) ratios
    closer a | (not . null . sm $ a) && (not . null . lg $ a) = if a - s <= l - a then s else l where
        s = maximum . sm $ a
        l = minimum . lg $ a
    closer a = a

readAspect :: Path Rel File -> IO (Ratio Word)
readAspect file = do
    x <- readImageWithMetadata . toFilePath $ file
    m <- case x of
            Left err        -> fail err
            Right (_, meta) -> pure meta
    let Just w = lookup Width m
        Just h = lookup Height m
    pure $ closestRatio $ w % h

newFileName :: Ratio Word -> Path Rel File -> IO (Path Rel File)
newFileName r file = parseRelFile (name ++ suffix ++ fileExtension file) where
        (fname, _) = breakOnEnd "." (pack . toFilePath $ file)
        name = init . unpack $ fname
        suffix = "-" ++ (show . numerator $ r) ++ "-" ++ (show . denominator $ r)

doWork :: IO ()
doWork = do
    a <- allImages
    putStrLn $ "Found " ++ show (length a) ++ " image files " ++ show extensions ++ "."
    forM_ a $ \p -> handle (\(_ :: IOException) -> putStr " ERROR!") $ do
        putStr $ "\nProcessing " ++ toFilePath p
        aspect <- readAspect p
        p' <- newFileName aspect p
        renameFile p p'
    putStrLn "\nDone!"
