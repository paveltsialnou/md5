module Test
  ( main
  ) where

import Crypto.Hash
import Data.ByteString.Lazy as ByteString
  ( ByteString
  , getContents
  , hPutStr
  , readFile
  )
import Data.Maybe (fromMaybe)
import Lib
import System.Directory
import System.IO as IO
import System.Process
import Test.QuickCheck
import Test.QuickCheck.Instances
import Test.QuickCheck.Monadic

md5 :: ByteString -> Digest MD5
md5 = hashlazy

prop_filePath :: ByteString -> Property
prop_filePath content =
  monadicIO $ do
    (file, handle) <- run $ openTempFile "./test" ".test"
    run $ ByteString.hPutStr handle content
    run $ hClose handle
    testMd5 <- run $ readMd5 file
    run $ removeFile file
    assert $ fromMaybe "-" (filePath testMd5) == file

prop_md5Sum :: ByteString -> Property
prop_md5Sum content =
  monadicIO $ do
    (file, handle) <- run $ openTempFile "./test" ".test"
    run $ ByteString.hPutStr handle content
    run $ hClose handle
    testMd5 <- run $ readMd5 file
    run $ removeFile file
    assert $ md5Sum testMd5 == md5 content

prop_show :: ByteString -> Property
prop_show content =
  monadicIO $ do
    let digest = hashlazy content
        md5 = Md5 {md5Sum = digest, filePath = Nothing}
    assert $ show md5 == show digest <> "  -"

prop_execContents :: String -> Property
prop_execContents content =
  monadicIO $ do
    stdFunc <- run $ readProcess "md5sum" [] content
    testFunc <- run $ readProcess "md5" [] content
    assert $ stdFunc == testFunc

prop_execFile :: String -> Property
prop_execFile content =
  monadicIO $ do
    (file, handle) <- run $ openTempFile "./test" ".test"
    run $ IO.hPutStr handle content
    run $ hClose handle
    stdFunc <- run $ readProcess "md5sum" [file] []
    testFunc <- run $ readProcess "md5" [file] []
    run $ removeFile file
    assert $ stdFunc == testFunc

main :: IO ()
main = do
  quickCheckWith stdArgs {maxSuccess = 500} prop_filePath
  quickCheckWith stdArgs {maxSuccess = 500} prop_md5Sum
  quickCheckWith stdArgs {maxSuccess = 50} prop_execContents
  quickCheckWith stdArgs {maxSuccess = 50} prop_execFile
  quickCheckWith stdArgs {maxSuccess = 500} prop_show
