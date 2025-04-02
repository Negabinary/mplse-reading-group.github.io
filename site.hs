--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid (mappend)

import Data.Bifunctor
import Data.ByteString.Char8 (ByteString, pack)
import Data.Function
import Data.List
import Data.Maybe
import Data.String
import Data.Text.Lazy hiding (reverse)
import Data.Text.Lazy.Encoding
import Hakyll
import Hakyll.Process
import Prelude hiding (FilePath)
import Text.Pandoc.Options
import Text.Regex.Base.RegexLike (getAllTextSubmatches)
import qualified Text.Regex.PCRE2 as PCRE

--------------------------------------------------------------------------------
main :: IO ()
main =
  hakyllWith config $ do
    match "images/*" $ do
      route idRoute
      compile copyFileCompiler
    match "css/*" $ do
      route idRoute
      compile compressCssCompiler
    match (fromList ["about.md"]) $ do
      route $ setExtension "html"
      compile
        $ pandocMdCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls
    match "posts/*" $ do
      route $ setExtension "html"
      compile
        $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html" postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls
    --create ["archive.html"] $ do
    --    route idRoute
    --    compile $ do
    --        posts <- recentFirst =<< loadAll "posts/*"
    --        let archiveCtx =
    --                listField "posts" postCtx (return posts) `mappend`
    --                constField "title" "Archives"            `mappend`
    --                defaultContext
    --        makeItem ""
    --            >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
    --
    --            >>= loadAndApplyTemplate "templates/default.html" archiveCtx
    --            >>= relativizeUrls
    let collate_schedules =
          \dir -> do
            route idRoute
            compile $ do
              let indexCtx =
                    let comparator (x :: Identifier) (y :: Identifier) =
                          let re :: ByteString =
                                "(\\d)+-(fall|winter|spring-summer)"
                           in fromJust $ do
                                xmatches <- Prelude.show x PCRE.=~~ re
                                ymatches <- Prelude.show y PCRE.=~~ re
                                let xm :: [String] =
                                      getAllTextSubmatches xmatches
                                let ym :: [String] =
                                      getAllTextSubmatches ymatches
                                -- HACK: We want to order the semesters as [winter, spring-summer, fall],
                                -- but [fall, spring-summer, winter] is the normal lexicographical order.
                                -- This hack is to avoid writing an explicit comparator.
                                let make_into_lexicographic_order semester
                                      | semester == "winter" = "xaa"
                                      | semester == "spring-summer" = "xab"
                                      | semester == "fall" = "xac"
                                return
                                  $ case (xm !! 1) `compare` (ym !! 1) of
                                      EQ ->
                                        let (d, d') =
                                              bimap
                                                make_into_lexicographic_order
                                                make_into_lexicographic_order
                                                (xm !! 2, ym !! 2)
                                         in d `compare` d'
                                      c -> c
                     in (listField
                           dir
                           (bodyField "schedule_body" `mappend` defaultContext)
                           (loadAll (fromString $ dir ++ "/*")
                              >>= return
                                    . reverse
                                    . (sortBy (comparator `on` itemIdentifier))))
                          `mappend` defaultContext
              getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
    match "index.html" $ collate_schedules "current_schedules"
    match "past.html" $ collate_schedules "past_schedules"
    let yaml2html shouldReverse = do
          route $ setExtension "html"
          compile
            $ execCompilerWith
                (execName "./schedule_yaml2html.pl")
                [HakFilePath, ProcArg shouldReverse]
                CStdOut
                >>= return . fmap (unpack . decodeUtf8)
                >>= loadAndApplyTemplate
                      "templates/schedule.html"
                      defaultContext
                >>= relativizeUrls
    match "current_schedules/*" (yaml2html "1")
            --(newExtOutFilePath "html")
      --route $ setExtension "html"
      --compile
      --  $ pandocMdCompiler
      --      >>= loadAndApplyTemplate "templates/schedule.html" defaultContext
      --      >>= relativizeUrls
    match "past_schedules/*" (yaml2html "0")
    match "templates/*" $ compile templateBodyCompiler

--------------------------------------------------------------------------------
-- leftover form `hakyll-init`
postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" `mappend` defaultContext

pandocMdCompiler =
  pandocCompilerWith (def {readerExtensions = pandocExtensions}) def

config :: Configuration
config =
  defaultConfiguration
    { destinationDirectory = "docs" -- for github pages
    }
