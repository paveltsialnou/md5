{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : $Header$
-- Description : for md5
-- Copyright   : (c) 2021 Pavel Tsialnou
-- License     : MIT
-- Maintainer  : paveltsialnou@icloud.com
-- Stability   : experimental
--
-- Provides 'Md5' data and 'readMd5' function.
--
-- @since 0.1.0.0
module Lib
  ( Md5(..)
  , readMd5
  ) where

import Control.Monad (fmap, return)
import Crypto.Hash (Digest, MD5, hashlazy)
import Data.ByteString.Lazy (ByteString(), getContents, readFile)
import Data.Function (($))
import Data.List (concat)
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import System.IO (FilePath, IO())
import Text.Show (Show, show)

-- |
-- Construct 'Md5' from either file path or standard input and MD5 digest.
--
-- >>> filePath <$> readMd5 "Setup.hs"
-- Just "Setup.hs"
-- >>> md5Sum <$> readMd5 "Setup.hs"
-- bad3a19586e908114369c22943337063
--
-- @since 0.1.0.0
data Md5 =
  Md5
    { filePath :: Maybe FilePath
    , md5Sum :: Digest MD5
    }

instance Show Md5 where
  show md = concat [show $ md5Sum md, "  ", fromMaybe "-" $ filePath md]

-- |
-- Read file either from path or standard input.
--
-- >>> readMd5 "Setup.hs"
-- bad3a19586e908114369c22943337063  Setup.hs
--
-- @since 0.1.0.0
readMd5 :: FilePath -> IO Md5
readMd5 fs = do
  (path, content) <-
    case fs of
      [] -> return (Nothing, getContents)
      fs -> return (Just fs, readFile fs)
  sum' <- fmap hashlazy content
  return Md5 {filePath = path, md5Sum = sum'}
