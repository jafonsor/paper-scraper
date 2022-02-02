{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module BenjaminsCSV (generateBenjaminsCSV) where

import Control.Monad (forM)
import Data.List (intersperse)
import System.IO (withFile, IOMode (WriteMode))


import Text.HTML.TagSoup
import Text.StringLike
import Data.Text.Lazy  (Text, pack)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy.IO as T
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Csv as Cassava
import qualified Data.ByteString.Lazy as BS

import Helpers

absoluteLink :: Text -> Text -> Text
absoluteLink root link = root <> "/" <> link

_BENJAMINS_JPCL_ROOT = "https://benjamins.com"
absoluteBenjaminLink = absoluteLink _BENJAMINS_JPCL_ROOT

_BENJAMINS_JPCL_MAIN_PAGE_URL = absoluteBenjaminLink "catalog/jpcl"


benjaminsMainPageTags = linkTags _BENJAMINS_JPCL_MAIN_PAGE_URL

data ParseInfoMainPage str = VolumeTitle str
                           | IssueLink str
                             deriving (Show)

isIssueLink (IssueLink _ ) = True
isIssueLink _ = False

extractLinkFromInfo (IssueLink link) = link
extractLinkFromInfo _ = undefined

data VolumeLink str = VolumeLink { title :: str, issueLinks :: [str] }
                      deriving (Show)

isVolumeStartTag :: StringLike str => Tag str -> Bool
isVolumeStartTag = (~== ("<h3>" :: String))
isIssueStartTag :: StringLike str => Tag str -> Bool
isIssueStartTag = (~== ("<div class='entry issue'>" :: String))

parseInfo :: (Show str, StringLike str) => [Tag str] -> [ParseInfoMainPage str]
parseInfo tags =
  let 
    rest = dropWhile (\t -> not (isVolumeStartTag t || isIssueStartTag t)) tags
  in
    if null rest
    then []
    else
      let
        (afterInfoParse, info) = 
          if isVolumeStartTag $ head rest
          then parseVolumeInfo rest
          else parseIssueInfo rest
      in
        info : parseInfo afterInfoParse

parseVolumeInfo tags =
  let
    (rest, volumeTagContent) = parseTagContent tags
    volumeTitle = innerText $ filter (isTagText) volumeTagContent
  in (rest, VolumeTitle volumeTitle)

parseIssueInfo tags =
  let
    (rest, issueContent) = parseTagContent tags
    atLink = fromAttrib "href" $ head $ dropWhile (~/= ("<a>" :: String)) issueContent
  in (rest, IssueLink atLink)

issuesContent :: StringLike str => [Tag str] -> [Tag str]
issuesContent tags =
  let
    rest = dropWhile (~/= ("<details id=issues>" :: String)) tags
    content = takeWhile (~/= ("</details>" :: String)) rest
  in
    content

generateVolumeLinks :: StringLike str => [ParseInfoMainPage str] -> [VolumeLink str]
generateVolumeLinks ((VolumeTitle str):rest) =  VolumeLink str issueLinks : generateVolumeLinks restTags
  where
    (links, restTags) = span isIssueLink rest
    issueLinks = map extractLinkFromInfo links
generateVolumeLinks [] = []
generateVolumeLinks _ = undefined

parseVolumeLinks allTags =
  let
    issueDiv = issuesContent allTags
    issueDivInfos = parseInfo issueDiv
  in 
    generateVolumeLinks issueDivInfos

-- issue page --

isNotIssueTitle = (~/= ("<h1 itemprop=name class=bktitle>" :: String))
parseIssueTitle tags = trimWhiteSpace $ innerText $ take 1 $ drop 1 $ dropWhile (isNotIssueTitle) tags


isNotArticleList = (~/= ("<ul class=articles>" :: String))
isNotArticleSeccion = (~/= ("<div class=article-type id=toc-section-title-3>" :: String))
parseArticleTags issueTags = articleList
  where
    (_, articleUL) = parseTagContent $ dropWhile isNotArticleList issueTags
    articleList = dropWhile isNotArticleSeccion articleUL -- ignore obituaries

isNotArticle = (~/= ("<a>" :: String))
parseIssueArticleLinks [] = []
parseIssueArticleLinks articleTagList =
    let (begin, end) = span isNotArticle articleTagList
    in case end of
          [] -> []
          (linkTag : rest)  -> (fromAttrib "href" linkTag) : parseIssueArticleLinks rest


-- article page --

isNotArticleTitle = (~/= ("<h1 class=bktitle>" :: String))
parseArticleTitle articlePage = trimWhiteSpace $ innerText titleTags
  where
    (rest, titleTags) = parseTagContent $ dropWhile isNotArticleTitle articlePage

isNotArticleSubtitle = (~/= ("<h2 class=bksubtitle>" :: String))
parseArticleSubtitle articlePage = trimWhiteSpace $ innerText subtitleTags
  where
    (rest, subtitleTags) = parseTagContent $ dropWhile isNotArticleSubtitle articlePage

isNotArticleAbstract = (~/= ("<div class=bkblurb itemprop=description>" :: String))
parseArticleAbstract articlePage = trimWhiteSpace $ innerText abstractTags
  where
    (rest, abstractTags) = parseTagContent $ dropWhile isNotArticleAbstract articlePage

isNotArticleKeywords = (~/= ("<span itemprop=keywords>" :: String))
parseArticleKeywords articlePage = trimWhiteSpace $ innerText keywordTags
  where
    (rest, keywordTags) = parseTagContent $ dropWhile isNotArticleKeywords articlePage

isNotArticleAuthor = (~/= ("<a itemprop=author>" :: String))
parseArticleAuthors [] = []
parseArticleAuthors articlePage = 
    if null beforeAuthorName || null beforeUniversityName
    then []
    else
      (authorName, universityName) : parseArticleAuthors beforeUniversityName
  where
    beforeAuthorName = dropWhile isNotArticleAuthor articlePage
    beforeUniversityName = dropWhile (~/= ("</span>" :: String)) beforeAuthorName

    authorName = innerText $ take 1 $ drop 1 $ beforeAuthorName
    universityName = innerText $ take 1 $ drop 1 $ beforeUniversityName



generateBenjaminsCSV :: IO ()
generateBenjaminsCSV = do
    mainPageTags <- benjaminsMainPageTags
    let volumeLinks = parseVolumeLinks mainPageTags
    
    withFile "benjamins_articles.csv" WriteMode (\handle -> do
      BS.hPutStr handle
                 $ Cassava.encode
                      ([("Issue", "Volume", "Title", "Subtitle", "Keywords", "Abstract", "Keywords", "Link")]
                       :: [(Text, Text, Text, Text, Text, Text, Text, Text)]) 
      forM volumeLinks (\volumeLink -> do
        let issueLks = issueLinks volumeLink
        
        forM issueLks (\issueLink -> do
          let volumeTitle = title volumeLink
          issueTags <- linkTags $ absoluteBenjaminLink issueLink
          let issueTitle = parseIssueTitle issueTags
          let (_, articleUL) = parseTagContent $ dropWhile isNotArticleList issueTags
          let articleLinks = parseIssueArticleLinks articleUL
          forM articleLinks $ \articleLink -> do
            articlePageTags <- linkTags $ absoluteBenjaminLink articleLink
            let articleTitle =  parseArticleTitle articlePageTags
            let articleSubTitle = parseArticleSubtitle articlePageTags
            let articleAuthors = parseArticleAuthors articlePageTags
            let articleAbstract = parseArticleAbstract articlePageTags
            let articleKeywords = parseArticleKeywords articlePageTags
            let aritcleAbsolutLink = absoluteBenjaminLink articleLink
            printTextList [volumeTitle, issueTitle, articleTitle, articleSubTitle, absoluteBenjaminLink articleLink]
            BS.hPutStr handle $ Cassava.encode [(issueTitle, volumeTitle, articleTitle, articleSubTitle, articleKeywords, articleAbstract, aritcleAbsolutLink)])))
    
    return ()