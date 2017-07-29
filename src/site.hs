{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid ((<>))
import Data.List (isSuffixOf)
import System.FilePath
import Hakyll

main :: IO ()
main = hakyllWith siteConf $ do
  match "images/*" $ do
    route   idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route   idRoute
    compile compressCssCompiler

  match "pages/*" $ do
    route   $ niceRoute ""
    compile $ pandocCompiler
          >>= loadAndApplyTemplate "templates/base.html" defaultContext
          >>= relativizeUrls

  match "posts/*" $ do
    route $ niceRoute "posts/"
    compile $ pandocCompiler
          >>= loadAndApplyTemplate "templates/post.html" postCtx
          >>= saveSnapshot "teaser"
          >>= loadAndApplyTemplate "templates/base.html" postCtx
          >>= relativizeUrls

  create ["posts.html"] $ do
    route   $ niceRoute ""
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let archiveCtx = listField "posts" postCtx (return posts)
                    <> constField "title" "Past Posts"
                    <> defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/posts.html" archiveCtx
        >>= loadAndApplyTemplate "templates/base.html"  archiveCtx
        >>= relativizeUrls
        >>= cleanIndexUrls

  match "notes/*" $ do
    route $ niceRoute "notes/"
    compile $ pandocCompiler
          >>= loadAndApplyTemplate "templates/post.html" postCtx
          >>= loadAndApplyTemplate "templates/base.html" postCtx
          >>= relativizeUrls

  create ["notes.html"] $ do
    route   $ niceRoute ""
    compile $ do
      notes <- recentFirst =<< loadAll "notes/*"
      let archiveCtx = listField "posts" postCtx (return notes)
                    <> constField "title" "Notes"
                    <> defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/notes.html" archiveCtx
        >>= loadAndApplyTemplate "templates/base.html"  archiveCtx
        >>= relativizeUrls
        >>= cleanIndexUrls

  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      notes <- recentFirst =<< loadAll "notes/*"
      let homePostCtx = teaserField "teaser" "teaser" <> postCtx
          indexCtx = listField "posts" homePostCtx (return (take 1 posts))
                  <> listField "notes" postCtx (return (take 6 notes))
                  <> constField "title" "Mostly Literate Machine Learning"
                  <> defaultContext

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/base.html" indexCtx
        >>= relativizeUrls
        >>= cleanIndexUrls

  match "templates/*" $ compile templateBodyCompiler

siteConf :: Configuration
siteConf = defaultConfiguration { deployCommand        = "bash src/deploy.sh"
                                , destinationDirectory = "generated/site"
                                , storeDirectory       = "generated/cache"
                                , tmpDirectory         = "generated/cache/tmp"
                                , providerDirectory    = "content"
                                , previewHost          = "0.0.0.0" }

postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" <> defaultContext

niceRoute :: String -> Routes
niceRoute prefix = customRoute $ \ident -> prefix ++ (takeBaseName . toFilePath $ ident) ++ "/index.html"

cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (withUrls cleanIndex)
  where idx = "index.html"
        cleanIndex url = let lengthIdx = if isSuffixOf idx url then length idx else 0
                         in take (length url - lengthIdx) url
