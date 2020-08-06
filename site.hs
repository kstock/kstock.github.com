--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged \"" ++ tag ++ "\""
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let num_posts = show $ length $ posts
            let ctx = constField "title" title
                      `mappend` listField "posts" (postCtxWithTags tags) (return posts)
                      `mappend` constField "num_posts" num_posts
                      `mappend` defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/tags.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    create ["tags.html"] $ do
        route idRoute
        compile $ do
            let num_tags = show $ length $ tagsMap tags
            let tagsCtx =
                    constField "title" "Tags collection" `mappend`
                    listField "tags" postCtx (traverse (makeItem . fst) (tagsMap tags)) `mappend`
                    constField "num_tags" num_tags `mappend`
                    defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/tag-list.html" tagsCtx
                >>= loadAndApplyTemplate "templates/default.html" tagsCtx
                >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    (postCtxWithTags tags)
            >>= loadAndApplyTemplate "templates/default.html" (postCtxWithTags tags)
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let num_posts = show $ length $ posts
            let archiveCtx =
                    listField "posts" (postCtxWithTags tags) (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    constField "num_posts" num_posts `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" (postCtxWithTags tags) (return posts) `mappend`
                    constField "title" "Home"                `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%Y-%m-%d" `mappend`
    defaultContext

postCtxWithTags :: Tags -> Context String
postCtxWithTags tags =
    tagsField "tags" tags `mappend`
    --(length (getTags tags))
    -- constField "numtags" "1" `mappend`
    postCtx
