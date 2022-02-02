{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module CambridgeCSV (generateCambridgeCSV) where

import Control.Monad (forM)
import Data.Foldable (fold)
import Data.List (intersperse)
import System.IO (withFile, hPutStrLn, IOMode (WriteMode))

import Text.HTML.TagSoup
import Text.StringLike
import Data.Text.Lazy  (Text, pack)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy.IO as T
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Csv as Cassava
import qualified Data.ByteString.Lazy as BS

import Helpers hiding (absoluteLink)

absoluteLink :: Text -> Text -> Text
absoluteLink root link = root <> link

_CAMBRIDGE_ROOT = "https://www.cambridge.org"
absoluteCambridgeLink = absoluteLink _CAMBRIDGE_ROOT

_CAMBRIDGE_MAIN_PAGE_URL :: Text
_CAMBRIDGE_MAIN_PAGE_URL = absoluteCambridgeLink "/core/journals/language-variation-and-change/all-issues"

-- main page --
parseIssueLinks :: [Tag Text] -> [Text]
parseIssueLinks =
    filter (T.isInfixOf "language-variation-and-change")
    . map (fromAttrib "href")
    . filter (~== ("<a class=row>" :: String))

-- issue page --
parseIssueTitle issuePageTags = trimWhiteSpace $ innerText issueTitleTags
  where
    (_, issueTitleTags) = parseTagContent $ dropWhile (~/= ("<h2 class=heading_07>" :: String)) issuePageTags

parseArticleLinks =
   filter 
     (\link -> not $ (T.isInfixOf "cover-and-front-matter" link)
                      || (T.isInfixOf "cover-and-back-matter" link))
   . map (fromAttrib "href")
   . filter (~== ("<a class=part-link>" :: String))

-- article page --

parseArticleTitle articlePageTags = trimWhiteSpace $ innerText articleTitleTags
  where
    (_, articleTitleTags) = parseTagContent $ dropWhile (~/= ("<h1 data-v-58bb6804>" :: String)) articlePageTags


parseArticleAbstract articlePageTags = trimWhiteSpace $ innerText articleAbstractTags
  where
    (_, articleAbstractTags) = parseTagContent $ dropWhile (~/= ("<div class=abstract>" :: String)) articlePageTags

parseArticleDOI =
  (\list ->
      if null list
      then ""
      else head list)
  . map (fromAttrib "href")
  . filter (~== ("<a data-v-1b245387 data-v-2957dcf9 class='app-link app-link__text app-link--accent'>" :: String))

generateCambridgeCSV = do
  mainPageTags <- linkTags _CAMBRIDGE_MAIN_PAGE_URL
  let issueLinks = parseIssueLinks mainPageTags

  withFile "cambridge_articles.csv" WriteMode (\handle -> do
    BS.hPutStr handle $ Cassava.encode ([("Issue", "Article Title", "Abstract", "DOI Link", "Cambridge Link")] :: [(Text, Text, Text, Text, Text)])
    forM issueLinks (\issueLink -> do
      issueTags <- linkTags $ absoluteCambridgeLink issueLink
      let issueTitle = parseIssueTitle issueTags
      let articleLinks = parseArticleLinks issueTags
      forM articleLinks $ \articleLink -> do
        let absoluteArticleLink = absoluteCambridgeLink articleLink
        articleTags <- linkTags absoluteArticleLink
        let articleTitle = parseArticleTitle articleTags
        let articleAbstract = parseArticleAbstract articleTags
        let articleDOI = parseArticleDOI articleTags
        printTextList [issueTitle, articleTitle, articleAbstract]
        BS.hPutStr handle $ Cassava.encode [(issueTitle, articleTitle, articleAbstract, articleDOI, absoluteArticleLink)]))