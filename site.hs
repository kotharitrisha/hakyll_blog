{-# LANGUAGE Arrows            #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Monoid     (mappend, mconcat)
import           Prelude         hiding (id)
import           System.Cmd      (system)
import           System.FilePath (replaceExtension, takeDirectory)
import qualified Text.Pandoc     as Pandoc
import           Hakyll

main :: IO ()
main = hakyll $ do

    match ("images/*") $ do
        route   idRoute
        compile copyFileCompiler

    -- Compress CSS
    match "css/*" $ do
        route idRoute
        compile compressCssCompiler

    -- Build tags
    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    -- Render each and every post
    -- NOTE:saving snapshots so that we can save an item at any point and return later 
    match "posts/*" $ do
        route   $ setExtension ".html"
        compile $ do
            pandocCompiler
                >>= saveSnapshot "content"
                >>= return . fmap demoteHeaders
                >>= loadAndApplyTemplate "templates/post.html" (postCtx tags)
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

    -- Post list
    create ["posts.html"] $ do
        route idRoute
        compile $ do
            list <- postList tags "posts/*" recentFirst
            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html"
                        (constField "title" "Posts" `mappend`
                            constField "posts" list `mappend`
                            defaultContext)
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

    -- Post tags
    tagsRules tags $ \tag pattern -> do
        let title = "Category:  " ++ tag
        route idRoute
        compile $ do
            list <- postList tags pattern recentFirst
            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html"
                        (constField "title" title `mappend`
                            constField "posts" list `mappend`
                            defaultContext)
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

        -- Create RSS feed 
        version "rss" $ do
            route   $ setExtension "xml"
            compile $ loadAllSnapshots pattern "content"
                >>= fmap (take 10) . recentFirst
                >>= renderAtom (feedConfiguration title) feedCtx

    -- Index page
    match "index.html" $ do
        route idRoute
        compile $ do
            list <- postList tags "posts/*" $ fmap (take 3) . recentFirst
            let indexContext = constField "posts" list `mappend`
                    field "tags" (\_ -> renderTagList tags) `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexContext
                >>= loadAndApplyTemplate "templates/default.html" indexContext
                >>= relativizeUrls

    -- Read templates
    match "templates/*" $ compile $ templateCompiler

    --Render the "About" and "Contact" pages
    match (fromList ["about.markdown", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

   --Compile links
    match "links/*" $ do
        route idRoute
	compile $ pandocCompiler


    -- Render RSS feed
    create ["rss.xml"] $ do
        route idRoute
        compile $ do
            loadAllSnapshots "posts/*" "content"
                >>= fmap (take 10) . recentFirst
                >>= renderAtom (feedConfiguration "All posts") feedCtx

   -- Render the links page
    match "links.html" $ do
        route idRoute
        compile $ do
            let linkCtx = field "links" $ \_ ->
                                linkList $ fmap (take 3) . recentFirst
            getResourceBody
                >>= applyAsTemplate linkCtx
                >>= loadAndApplyTemplate "templates/default.html" defaultContext 
		>>= relativizeUrls


--Creating the post "context" which has a time, date, and tags
postCtx :: Tags -> Context String
postCtx tags = mconcat
    [ modificationTimeField "mtime" "%U"
    , dateField "date" "%B %e, %Y"
    , tagsField "tags" tags
    , defaultContext
    ]

feedCtx :: Context String
feedCtx = mconcat
    [ bodyField "description"
    , defaultContext
    ]

feedConfiguration :: String -> FeedConfiguration
feedConfiguration title = FeedConfiguration
    { feedTitle       = "Trisha's thoughts"
    , feedDescription = "Personal blog of Trisha"
    , feedAuthorName  = "Trisha Kothari"
    , feedAuthorEmail = "kotharitrisha@gmail.com"
    , feedRoot        = "http://www.seas.upenn.edu/~kotharit"
    }


--Applying post-item template on all posts
postList :: Tags -> Pattern -> ([Item String] -> Compiler [Item String])
         -> Compiler String
postList tags pattern preprocess' = do
    postItemTpl <- loadBody "templates/postitem.html"
    posts       <- preprocess' =<< loadAll pattern
    applyTemplateList postItemTpl (postCtx tags) posts


--Applying link template on all links
linkList :: ([Item String] -> Compiler [Item String]) -> Compiler String
linkList sortFilter = do
    links   <- sortFilter =<< loadAll "links/*"
    itemTpl <- loadBody "templates/link-item.html"
    list    <- applyTemplateList itemTpl defaultContext links
    return list
