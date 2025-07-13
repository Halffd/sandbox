
{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Simple
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as L8
import Text.Parsec
import Text.Parsec.String
import Conduit
import System.FilePath
import System.IO
import System.Directory
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad

page :: Request -> IO L8.ByteString
page = fmap getResponseBody . httpLBS

urls :: Parser [String]
urls = many . try . fmap (("https://"; <>) . fixDomain) $ do
  everything (string "class=\"fileText\"")
  everything (string "href=\"")
  everything (char '"')

everything :: Parser s -> Parser String
everything = manyTill anyChar . try

fixDomain :: String -> String
fixDomain = ("i.4cdn.org" <>) . dropWhile (/= '/') . drop 2

fetch :: String -> IO ()
fetch url = do
  putStrLn $ "Fetching " <> url
  request <- parseRequest url
  (runResourceT . httpSink request . const . sinkFile . takeFileName) url

split :: Int -> [a] -> [[a]]
split n xs | (l,r) <- splitAt n xs =
  case r of
    [] -> [l]
    _ -> l : split n r

main :: IO ()
main = do
  let currentDir = "gifs"
  thread <- page "https://boards.4chan.org/gif/thread/20750401";

  currentDirExists <- doesDirectoryExist currentDir
  unless currentDirExists $
    createDirectory currentDir

  case parse urls "" (L8.unpack thread) of
    Left e -> print e
    Right r ->
      mapM_ (withCurrentDirectory currentDir . mapConcurrently_ fetch) (split 8 r)