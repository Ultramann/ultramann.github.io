{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (mapM_, when)
import Data.Monoid ((<>))
import Data.List (isSuffixOf)
import System.Environment (getArgs)
import System.FilePath (takeBaseName)
import Hakyll

clean :: IO ()
clean = mapM_ remove ["site", "cache"]
  where remove dir = let path = "generated/" ++ dir in do 
          putStrLn $ "Removing " ++ path ++ "..."
          removeDirectory path

main :: IO ()
main = do
  (arg:_) <- getArgs
  let notes = "notes/*"
      devel = arg == "watch"
      posts = fromGlob $ "posts/" ++ if devel then "**" else "*"
  when devel clean

  hakyllWith siteConf $ do
    match ("images/**" .||. "js/*") $ do
      route   idRoute
      compile copyFileCompiler

    match "css/*" $ do
      route   idRoute
      compile compressCssCompiler

    match "pages/*" $ do
      route   $ niceRoute ""
      compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/base.html" defaultContext

    match posts $ do
      route   $ niceRoute "posts/"
      compile $ pandocCompiler
            >>= saveSnapshot "teaser"
            >>= loadAndApplyTemplate "templates/byte.html" byteCtx
            >>= loadAndApplyTemplate "templates/base.html" byteCtx

    match notes $ do
      route   $ niceRoute "notes/"
      compile $ pandocCompiler
            >>= saveSnapshot "teaser"
            >>= loadAndApplyTemplate "templates/byte.html" byteCtx
            >>= loadAndApplyTemplate "templates/base.html" byteCtx

    create ["posts.html"] $ do
      route   $ niceRoute ""
      compile $ do
        chronPosts <- recentFirst =<< loadAll posts
        let archiveCtx = listField "bytes" byteCtx (return chronPosts)
                      <> constField "title" "Posts"
                      <> defaultContext

        makeItem ""
          >>= loadAndApplyTemplate "templates/posts.html" archiveCtx
          >>= loadAndApplyTemplate "templates/base.html"  archiveCtx
          >>= relativizeUrls
          >>= cleanIndexUrls

    create ["notes.html"] $ do
      route   $ niceRoute ""
      compile $ do
        chronNotes <- recentFirst =<< loadAll notes
        let archiveCtx = listField "bytes" byteCtx (return chronNotes)
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
        chronPosts <- recentFirst =<< loadAll posts
        chronNotes <- recentFirst =<< loadAll notes
        let indexCtx = listField "posts" byteCtx (return (take 1 chronPosts))
                    <> listField "notes" byteCtx (return (take 6 chronNotes))
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

byteCtx :: Context String
byteCtx = teaserField "teaser" "teaser"
       <> dateField "date" "%b %e, %Y"
       <> defaultContext

niceRoute :: String -> Routes
niceRoute prefix = customRoute $ \ident -> prefix ++ (takeBaseName . toFilePath $ ident) ++ "/index.html"

cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (withUrls cleanIndex)
  where idx = "index.html"
        cleanIndex url = let lengthIdx = if isSuffixOf idx url then length idx else 0
                         in take (length url - lengthIdx) url
