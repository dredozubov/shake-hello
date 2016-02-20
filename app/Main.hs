{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad.IO.Class
import qualified Data.ByteString as B
import Data.Yaml
import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import GHC.Generics (Generic)

data Image = Image
  { name :: String
  , id :: String
  , release :: Int
  , image :: String
  , codename :: String
  } deriving (Show, Eq, Generic)

instance FromJSON Image

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="build"} $ do
  want ["build/images"]

  "build/images" %> \out -> do
    fileContent <- liftIO (B.readFile "images.yml")
    let images :: Either String [Image]
        images = decodeEither fileContent
    liftIO (print images)
    liftIO (writeFile "build/images" "Ok")
