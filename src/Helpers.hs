{-# LANGUAGE OverloadedStrings #-}

module Helpers where

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Data.Text.Lazy  (Text, pack)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy.IO as T
import Text.HTML.TagSoup
import Text.StringLike
import Data.ByteString.Lazy.Char8 (unpack)

import Data.Foldable (fold)
import Data.List (intersperse)
import Data.Char (isSpace)

-- http helpers --

openURL :: Text -> IO Text
openURL url = do
  request <- parseRequest $ T.unpack url
  manager <- newManager (tlsManagerSettings { managerResponseTimeout = responseTimeoutNone })
  (T.decodeUtf8 . responseBody) <$> httpLbs request manager
  

linkTags = (parseTags <$>) . openURL


-- io helpers --

printTextList :: [Text] -> IO ()
printTextList = T.putStrLn . fold . intersperse ", "

-- tagsoup helpers --

trimWhiteSpace text = removeExtraWhiteSpace withSpacesOnly
  where
    withSpacesOnly = T.strip $ T.map (\c -> if isSpace c then ' ' else c) text
    removeExtraWhiteSpace :: Text -> Text
    removeExtraWhiteSpace = T.foldr merge ""
      where
        merge :: Char -> Text -> Text
        merge c text =
          if T.null text
             || (not $ isSpace c)
             || (isSpace c
                 && not (isSpace (T.head text)))
          then T.singleton c <> text
          else text


-- assumes head of tags is tagOpen. takesWhile the tag is not closed
-- (rest, content)
parseTagContent :: StringLike str => [Tag str] -> ([Tag str], [Tag str])
parseTagContent [] = ([], [])
parseTagContent tags@(t:ts) =
  let
    parseUntilClose tags =
      if null t2
      then (tags, [])
      else
        if isTagOpen $ head t2
        then let (t3, innerTagContent) = parseTagContent t2
                 (t4, tagsUntilClose) = parseUntilClose t3
              in (t4, contentUntilTag ++ innerTagContent ++ tagsUntilClose)
        else (tail t2, contentUntilTag ++ [head t2])
      where
        (contentUntilTag, t2) = span (\t -> not ((isTagOpen t) || (isTagClose t))) tags

    (trest, tcontent) = parseUntilClose ts
  in
    (trest, t : tcontent)