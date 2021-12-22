{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : $Header$
-- Description : for md5
-- Copyright   : (c) 2021 Pavel Tsialnou
-- License     : MIT
-- Maintainer  : paveltsialnou@icloud.com
-- Stability   : experimental
--
-- Provides 'main' function.
--
-- @since 0.1.0.0
module Main
  ( main
  ) where

import Control.Monad ((>>=), mapM, mapM_)
import Data.List (null)
import Lib (readMd5)
import System.Environment (getArgs)
import System.IO (IO(), print)

-- |
-- Parse app args, read files/stdin and print MD5 for them.
--
-- @since 0.1.0.0
main :: IO ()
main = do
  args <- getArgs
  if null args
    then readMd5 [] >>= print
    else mapM readMd5 args >>= mapM_ print
