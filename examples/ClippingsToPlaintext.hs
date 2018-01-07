{-# LANGUAGE RecordWildCards, LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (putStr)
import Data.Bifunctor (Bifunctor(bimap))
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import System.Directory
import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure)
import System.IO (hPutStr, stderr)
import Text.Kindle.Clippings (Clipping(..), Document(..), Content(..), readClippings)
import Text.Parsec (parse)
import qualified Data.Text as T

(<$$>) = fmap . fmap

getClippings :: String -> Either String [Clipping]
getClippings = bimap show catMaybes
             . parse readClippings []

printFilename :: Clipping -> String
printFilename Clipping{..} = T.unpack . sanitizeFilename . T.pack $ original
  where
    original =
      show document <> sep <>
      show position <> sep <>
      show date <> ".txt"

    sep = " | "

sanitizeFilename :: T.Text -> T.Text
sanitizeFilename =
  T.replace "?" "" .
  T.replace "/" "|" .
  T.replace "\xfeff" ""

writePlaintext :: Clipping -> IO ()
writePlaintext c = do
  let filename = printFilename c
  isExist <- doesFileExist filename
  if not isExist
  then writeFile filename (show c)
  else pure ()

main :: IO ()
main = head <$> getArgs >>= getClippings <$$> readFile >>= \case
  Left err -> hPutStr stderr err >> exitFailure
  Right cs -> mapM_ writePlaintext cs >> exitSuccess
