--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative (pure, empty, (<$>), liftA, liftA2)
import           Control.Monad (join, forM, liftM, liftM2, filterM)
import           Data.Maybe          (maybe, catMaybes, maybeToList, isJust, fromJust, fromMaybe)
import           Data.Monoid         (Monoid, mempty, mconcat, (<>))
import           Data.List (find, isPrefixOf)
import           Data.List.Split (splitOn)
import qualified Data.Map.Strict as Map
import           Data.String (fromString)  
import           Hakyll hiding (listField)
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Text as T   (pack, unpack, breakOn)
import qualified Data.Text.Encoding as E
import           Text.Jasmine
import           Debug.Hood.Observe
import           System.FilePath     (replaceExtension, takeDirectory, (</>))

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "_assets/images/**" $ do
        route (gsubRoute "_assets/" (const ""))
        compile copyFileCompiler

    match "_assets/fonts/fontello/*" $ do
        route (gsubRoute "_assets/" (const ""))
        compile copyFileCompiler

    match "_assets/fonts/fontello.css" $ compile getResourceString

    match "_assets/css/*.css" $ compile getResourceString
--    match ( "_assets/css/*" .&&. ( complement "_assets/css/*.min.css" ) ) $ compile compressCssCompiler 
    create ["css/style.css"] $ do 
        route (gsubRoute "_assets/" (const "")) 
        compile $ do 
            bootstrap <- load "_assets/css/bootstrap.min.css"
            main <- load "_assets/css/main.css"
            iilab <- load "_assets/css/iilab.css"
            gray <- load "_assets/css/gray.css"
            carousel <- load "_assets/css/owl.carousel.css"
            transitions <- load "_assets/css/owl.transitions.css"
            animate <- load "_assets/css/animate.min.css"
            font <- load "_assets/fonts/fontello.css"
            makeItem $ unlines (map itemBody (bootstrap:main:iilab:gray:carousel:transitions:animate:font:[] :: [Item String]))

--  Copy IE HTML5 shims
    match ("_assets/js/ie/*" .||. "_assets/js/*.map" .||. "_assets/js/onscroll.js") $ do
        route (gsubRoute "_assets/" (const ""))
        compile copyFileCompiler

    match "_assets/js/*.js" $ compile getResourceString
--    match ( "_assets/js/*" .&&. ( complement "_assets/js/*.min.js" ) ) $ compile compressJsCompiler 
    create ["js/scripts.js"] $ do 
        route (gsubRoute "_assets/" (const "")) 
        compile $ do 
            jquery <- load "_assets/js/jquery.min.js"
            easing <- load "_assets/js/jquery.easing.1.3.min.js"
            bootstrap <- load "_assets/js/bootstrap.min.js"
            hover <- load "_assets/js/bootstrap-hover-dropdown.min.js"
            skrollr <- load "_assets/js/skrollr.min.js"
            skrollrstyle <- load "_assets/js/skrollr.stylesheets.min.js"
            waypoints <- load "_assets/js/waypoints.min.js"
            waypointssticky <- load "_assets/js/waypoints-sticky.min.js"            
            carousel <- load "_assets/js/owl.carousel.min.js"
            form <- load "_assets/js/jquery.form.js"
            validate <- load "_assets/js/jquery.validate.min.js"
            respond <- load "_assets/js/respond.min.js"
            isotope <- load "_assets/js/jquery.isotope.min.js"
            infinite <- load "_assets/js/jquery.infinitescroll.min.js"
            easytabs <- load "_assets/js/jquery.easytabs.min.js"
            viewport <- load "_assets/js/viewport-units-buggyfill.js"            
            scripts <- load "_assets/js/theme_scripts.js"
            iilab <- load "_assets/js/iilab.js"
--            concatItems (core:items) >>= withItemBody compressJsCompiler
            makeItem $ unlines (map itemBody (((jquery:easing:bootstrap:hover:skrollr:skrollrstyle:waypoints:waypointssticky:carousel:form:validate:respond:isotope:infinite:easytabs:viewport:scripts:iilab:[])) :: [Item String]))

{--    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "_templates/default.html" defaultContext
            >>= relativizeUrls
--}
    match "_posts/news/*" $ do
        route $ setExtension "html" `composeRoutes` (gsubRoute "_posts/" (const "")) 
        compile $ do
            projects <- recentFirst =<< loadAll ("_posts/projects/*" .&&. hasVersion "raw")
            let blogCtx =
                    listField "projects" projectCtx (return projects) <>
                    postCtx <>
                    siteCtx <>
                    defaultContext

            pandocCompiler
                >>= saveSnapshot "blog-content"
                >>= loadAndApplyTemplate "_templates/news/content.html"  blogCtx
                >>= loadAndApplyTemplate "_templates/news/post.html" blogCtx
                >>= loadAndApplyTemplate "_templates/default.html" blogCtx
                >>= relativizeUrls

    match "_posts/news/*" $ version "raw" $ do
        route   idRoute
        compile getResourceBody

    match "_posts/projects/*" $ do
        route $ setExtension "html" `composeRoutes` (gsubRoute "_posts/" (const ""))
        compile $ do
            id' <- getUnderlying
            metadata <- getMetadata id'
            projId <- return $ fromJust $ Map.lookup "slug" metadata
            projects <- recentFirst =<< loadAll ("_posts/projects/*" .&&. hasVersion "raw")
            posts <- projectPost projId =<< recentFirst =<< loadAllSnapshots ( "_posts/news/*" .&&. hasNoVersion ) "blog-content"
--            posts <- projectPost projId =<< recentFirst =<< loadAll ( "_posts/news/*" .&&. hasVersion "raw")
            let projCtx =
                    listField "projects" projectCtx (return projects) <>
                    listField "posts" postCtx (return posts) <>
                    projectCtx <>
                    siteCtx <>
                    defaultContext

            pandocCompiler
                >>= saveSnapshot "project-content"
                >>= loadAndApplyTemplate "_templates/project.html" projCtx
                >>= loadAndApplyTemplate "_templates/default.html" projCtx
                >>= relativizeUrls

    match "_posts/projects/*" $ version "raw" $ do
        route   idRoute
        compile getResourceBody

    create ["index.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAllSnapshots ( "_posts/news/*" .&&. hasNoVersion ) "blog-content"
            projects <- recentFirst =<< loadAll ("_posts/projects/*" .&&. hasVersion "raw")
            let indexCtx =
                    listField "posts" postCtx (return posts) <>
                    listField "projects" projectCtx (return projects) <>
                    constField "title" "Innovative Technology for Social Impact" <>
                    siteCtx <>
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "_templates/index.html" indexCtx
                >>= loadAndApplyTemplate "_templates/default.html" indexCtx
                >>= relativizeUrls

    -- Render blog index feed
    create ["news.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAllSnapshots ( "_posts/news/*" .&&. hasNoVersion ) "blog-content"
            projects <- recentFirst =<< loadAll ("_posts/projects/*" .&&. hasVersion "raw")
            let ctx =
                    listField "posts" postCtx (return posts) <>
                    listField "projects" projectCtx (return projects) <>
                    constField "title" "Blog" <>
                    siteCtx <>
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "_templates/news.html" ctx
                >>= loadAndApplyTemplate "_templates/default.html" ctx
                >>= relativizeUrls

    -- Render RSS feed
    create ["rss.xml"] $ do
        route idRoute
        compile $ do
            loadAllSnapshots "_posts/*" "content"
                >>= fmap (take 10) . recentFirst
                >>= renderRss (feedConfiguration "All posts") feedCtx

    match "_templates/**" $ compile templateCompiler

    where
      
        compressJsCompiler :: Compiler (Item String)
        compressJsCompiler = fmap jasmin <$> getResourceString
        
        jasmin :: String -> String
        jasmin src = LB.unpack $ minify $ LB.fromChunks [(E.encodeUtf8 $ T.pack src)] 

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "day" "%d" <>
    dateField "month" "%b" <>
    dateField "year" "%Y" <>
    dateField "date" "%B %e, %Y" <>
    listFieldWith "authors" authorCtx getAuthors <>
    lessField "less" "blog-content" <>
    moreField "more" "blog-content" <>
    shorturlCtx <>
    testCtx <>
    formatCtx <>
    postformatCtx <>
    iconCtx <>
    postImagesCtx <>
    siteCtx <>
    defaultContext

--------------------------------------------------------------------------------
-- | A context with "teaser" key which contain a teaser of the item.
-- The item is loaded from the given snapshot (which should be saved
-- in the user code before any templates are applied).
lessField :: String           -- ^ Key to use
            -> Snapshot         -- ^ Snapshot to load
            -> Context String   -- ^ Resulting context
lessField key snapshot = field key $ \item -> do
    body <- itemBody <$> loadSnapshot (itemIdentifier item) snapshot
    case needlePrefix "<!--more-->" body of
        Nothing -> return body
        Just t -> return t

--------------------------------------------------------------------------------
-- | A context with "teaser" key which contain a teaser of the item.
-- The item is loaded from the given snapshot (which should be saved
-- in the user code before any templates are applied).
moreField :: String           -- ^ Key to use
            -> Snapshot         -- ^ Snapshot to load
            -> Context String   -- ^ Resulting context
moreField key snapshot = field key $ \item -> do
    body <- itemBody <$> loadSnapshot (itemIdentifier item) snapshot
    case needleSuffix "<!--more-->" body of
        Nothing -> return ""
        Just t -> return t


--------------------------------------------------------------------------------
projectPost' :: Compiler Identifier -> [Item String] -> Compiler [Item String]
projectPost' proj items = do
    mapM makeItem [] 

--------------------------------------------------------------------------------
projectPost :: String -> [Item String] -> Compiler [Item String]
projectPost proj items = do
  -- itemsWithTime is a list of couple (date,item)
  projectPosts <- flip filterM items $ \item -> do
    -- curProj <- getMetadataField' projId "title"
    -- getItemUTC will look for the metadata "published" or "date"
    -- then it will try to get the date from some standard formats
    postProjMetadata <- getMetadata (setVersion (Just "raw") (itemIdentifier item) )
    let project = Map.lookup "projects" postProjMetadata 
    -- project <- getMetadataField (itemIdentifier item) "project"
    debugCompiler $ "projectPost - " <> proj <> " " <> show project
    case project of
        Just postProj -> if (proj == postProj) then return True else return False
        Nothing -> return False
  -- we return a sorted item list
  return projectPosts

--------------------------------------------------------------------------------

shorturlCtx :: Context a
shorturlCtx = field "shorturl" $ \item -> do
    url <- getMetadataField' (itemIdentifier item) "url"
    let shorturl = trim $ take 27 $ url 
    return $ shorturl <> "..."
--------------------------------------------------------------------------------

authorCtx :: Context Staff
authorCtx = field "key" ( return . key . itemBody ) <>
            field "name" ( return . name . itemBody ) <>
            field "description" ( return . description . itemBody ) <>
            field "title" ( return . title . itemBody ) <>
            field "twitter" ( return . twitter . itemBody ) <>
            field "linkedin" ( return . linkedin . itemBody ) <>
            field "github" ( return . github . itemBody ) <>
            field "site" ( return . site . itemBody )

getAuthors :: Item String -> Compiler [ Item Staff ]
getAuthors item = do 
    author_key <- getMetadataField' (itemIdentifier item) "author"
    mapM makeItem $ filter (isKey author_key) staff
    
isKey :: String -> Staff -> Bool 
isKey k stf = ( k == key stf )

--------------------------------------------------------------------------------

testCtx :: Context a
testCtx = functionField "test" $  \x _ -> case x of
                                  _    -> return "test empty!"

--------------------------------------------------------------------------------
formatCtx :: Context String
formatCtx = field "blog" ( getFormat "blog" ) <>
            field "social" ( getFormat "social" ) <> 
            field "social" ( getFormat "link" ) <> 
            field "tech" ( getFormat "tech" ) 

getFormat :: String -> Item String -> Compiler String
getFormat str item = do 
    format <- getMetadataField' (itemIdentifier item) "format"
    if (== str) format then return format else empty


--------------------------------------------------------------------------------
postformatCtx :: Context a
postformatCtx = field "post-format" $ \item -> do
    format <- getMetadataField (itemIdentifier item) "format"
    -- Inspect metadata and decide title based on that
    case format of 
        Just "blog" -> return "blog"
        Just "social" -> return "social"
        Just "link" -> return "social"
        Just "tech" -> return "tech"
        Just "format" -> return "wat?"
        Nothing -> return "edit"

--------------------------------------------------------------------------------
iconCtx :: Context a
iconCtx = field "icon-format" $ \item -> do
    format <- getMetadataField (itemIdentifier item) "format"
    -- Inspect metadata and decide title based on that
    case format of 
        Just "blog" -> return "edit"
        Just "social" -> return "quote"
        Just "link" -> return "quote"
        Just "tech" -> return "tools"
        Just "format" -> return "wat?"
        Nothing -> return "edit"


--------------------------------------------------------------------------------
projectLinksCtx :: Context String
projectLinksCtx = listFieldWith' "project-links" (projectLinkCtx) getLinks

getLinks :: Item String -> Compiler [Item (String,String)]
getLinks item = do
        maybelinks <- getMetadataField (itemIdentifier item) "links"
        maybetitles <- getMetadataField (itemIdentifier item) "links-title"
        links <- return $ map trim $ maybe [] (splitOn ",") maybelinks 
        titles <- return $ map trim $ maybe [] (splitOn "|") maybetitles
        projectlinks <- return $ zipPad links titles
        mapM makeItem projectlinks

projectLinkCtx :: Context (String, String)
projectLinkCtx = mconcat [ field "link" $ return . fst . itemBody 
                         , field "title" $ return . snd . itemBody 
                         ]

--------------------------------------------------------------------------------
projectPartnersCtx :: Context String
projectPartnersCtx = listFieldWith' "project-partners" (projectPartnerCtx) getPartners

getPartners :: Item String -> Compiler [Item (String,String)]
getPartners item = do
        maybepartners <- getMetadataField (itemIdentifier item) "partners"
        maybelinks <- getMetadataField (itemIdentifier item) "partners-link"
        partners <- return $ map trim $ maybe [] (splitOn ",") maybepartners 
        links <- return $ map trim $ maybe [] (splitOn "|") maybelinks
        projectpartner <- return $ zipPad partners links
        mapM makeItem projectpartner

projectPartnerCtx :: Context (String, String)
projectPartnerCtx = mconcat [ field "partner" $ return . fst . itemBody 
                            , field "link" $ return . snd . itemBody 
                            ]

--------------------------------------------------------------------------------
postImagesCtx :: Context String
postImagesCtx = listFieldWith "images" (imageCtx) getImages

getImages :: Item String -> Compiler [Item (String,String)]
getImages item = do
        maybesrc <- getMetadataField (itemIdentifier item) "images"
        maybecaption <- getMetadataField (itemIdentifier item) "images-caption"
        srcs <- return $ map trim $ maybe [] (splitOn ",") maybesrc 
        captions <- return $ map trim $ maybe [] (splitOn "|") maybecaption
        imgs <- return $ zipPad srcs captions
        mapM makeItem imgs

imageCtx :: Context (String, String)
imageCtx = mconcat [ field "src" $ return . fst . itemBody 
                   , field "caption" $ return . snd . itemBody 
                   ]

--------------------------------------------------------------------------------
projectCtx :: Context String
projectCtx =
    dateField "date" "%B %e, %Y" <>
    teaserField "teaser" "project-content" <>
    projectPartnersCtx <>
    projectLinksCtx <>
    postImagesCtx <>
    siteCtx <>
    defaultContext

--------------------------------------------------------------------------------
feedCtx :: Context String
feedCtx = mconcat
    [ bodyField "description"
    , defaultContext
    ]

--------------------------------------------------------------------------------
feedConfiguration :: String -> FeedConfiguration
feedConfiguration title = FeedConfiguration
    { feedTitle       = "iilab - " ++ title
    , feedDescription = "iilab - blog feed"
    , feedAuthorName  = "iilab"
    , feedAuthorEmail = "contact@iilab.org"
    , feedRoot        = "https://iilab.org"
    }

--------------------------------------------------------------------------------

siteCtx :: Context String
siteCtx = mconcat
    [ constField "site.title" "iilab"
    , constField "site.url" "iilab.org"
    , constField "site.title" "iilab"
    , constField "site.tagline" "Innovative Technology for Social Impact"
    , constField "site.subtitle" "information innovation lab"
    , constField "site.description" "We harness information innovation to help your social good initiatives improve their human impact."
    , constField "site.contact.email" "contact@iilab.org"
    , constField "site.contact.phone" "+44 7429 144 691"
    , constField "site.contact.address1" "iilab London<br> Unit 36, 88-90 Hatton Garden<br> London EC1N 8PN - United Kingdom"
    , constField "site.contact.address2" "iilab Berlin<br> Straßburger Straße 29<br>10405 Berlin - Germany"
    , constField "site.contact.registration" "Registered in England & Wales. <br> Reg. No: 08332887. VAT No: GB172453217"
    , constField "author.name" "Jun Matsushita"
    , constField "author.github" "jmatsushita"
    , constField "author.email" "contact@iilab.org"
    , constField "favicon" "images/favicon.ico"
    , constField "icon" "images/favicon.ico"
    , constField "feed" "/rss.xml"
    , constField "shortcut_icon" "images/favicon.ico"
    , constField "social.gplus" "https://plus.google.com/b/106011859086478758954/106011859086478758954"
    , constField "social.linkedin" "http://www.linkedin.com/company/iilab"
    , constField "social.github" "https://github.com/iilab"
    , constField "social.facebook" "http://www.facebook.com/iilab.org"
    , constField "social.twitter" "http://twitter.com/iilab"
    , constField "social.envelope" "mailto:contact@iilab.org"
    , listField "programs" 
                ( mconcat [ field "key" $ return . fst . itemBody
                          , field "name" $ return . snd . itemBody ] )
                ( return [ Item (fromString "innovation") ("innovation", "Information Innovation") 
                         , Item (fromString "rights") ("rights", "Rights Enabling Tech") 
                         , Item (fromString "community") ("community", "Community Enabling Tech") ])
    , constField "hero.tagline" "Innovative Technology<br/> for Social Impact"
    , constField "hero.description" "We harness information innovation<br/>to help your social good initiatives<br/> improve their human impact"
    , constField "hero.button" "Contact Us"
    , constField "about.title" "What we stand for."
    , constField "about.description" "iilab - information innovation lab - stands for the respect and development of fundamental rights of communities everywhere around the world. We empower the social economy with innovation and technology."
    , constField "about.right.title" "Unlock Potential"
    , constField "about.right.description" "Help bridge the gap between the current use of innovation and technology and their potential to create social impact."
    , constField "about.center.title" "Inspire Talent"
    , constField "about.center.description" "Develop new skills for new tools, and help organisations and individuals shift their perspective and harness complex information ecosystems."
    , constField "about.left.title" "Make it Work"
    , constField "about.left.description" "Unpack, open and innovate, share experiences and co-create sustainable social impact;"
    , constField "work.title" "Our work"
    , constField "work.description" "Our current and past projects"
    , constField "team.title" "Our Team"
    , constField "team.description" "We harness multi-disciplinarity, systems and design thinking to create services and products that improve the lives of citizens."
    , constField "contributors.title" "Our Contributors"
    , constField "contributors.description" "We are stronger because we're part of the open source community. We're working with passionate and visonary developers, designers and thinkers from the open source and free culture movements."
    , listField "staff"
                ( mconcat [ field "key" $ return . key . itemBody 
                          , boolField "offset" $ offset . itemBody
                          , field "name" $ return .name . itemBody 
                          , field "title" $ return .title . itemBody 
                          , field "description" $ return . description . itemBody
                          , field "twitter" $ return . twitter . itemBody
                          , field "linkedin" $ return . linkedin . itemBody
                          , field "github" $ return . github . itemBody
                          , field "site" $ return . site . itemBody ] )
                ( mapM makeItem staff )
    , listField "contributors"
                ( mconcat [ field "key" $ return . Map.findWithDefault "" "key" . itemBody 
                          , field "name" $ return . Map.findWithDefault "" "name" . itemBody 
                          , field "description" $ return . Map.findWithDefault "" "description" . itemBody
                          , field "projects" $ return . Map.findWithDefault "" "projects" . itemBody
-- 
-- It's unclear how I can nest listFields 
-- listField :: String -> Context a -> Compiler [Item a] -> Context b
-- makeItem :: a -> Compiler (Item a)
--
-- mapM :: Monad m => (a -> m b) -> [a] -> m [b]
-- sequence :: Monad m => [m a] -> m [a]

-- 
-- mapM makeItem :: [a] -> Compiler [Item a]
-- 
-- I'd like to transform the Map item into a list of its own that can be used as the Compilter [Item a] that would be fed to the inner listField.
-- So maybe I need to provide a curried function as the third parameter of listField that will compose with what is fed to it by the outer listField 
-- The type signature of the curred function would be:
-- Item a -> Compiler [Item a]
-- 
-- itemBody :: Item a -> a
-- Map.findWithDefault "" "affiliations" :: Map.Map [Char] [Char] -> [Char]
-- (splitOn ",") :: String [String]
-- 
-- It's a partial applicaiton thing
-- When the last argument is not passed i.e. 
-- listField "name" someCtx 
--
-- then the function seems properly curried and waits for its third argument provided by the sequence (by the implementation of listField?).
-- The key was to use listFieldWith !!!
--
                          , listFieldWith "affiliations" ( mconcat [ field "key" $ return . T.unpack . fst . (T.breakOn $ T.pack "|") <$> T.pack . itemBody
                                                               , field "site" $ return . T.unpack . snd . (T.breakOn $ T.pack "|") <$> T.pack . itemBody ] ) 
                                                         ( mapM makeItem . (splitOn ",") . Map.findWithDefault "" "affiliations" . itemBody ) 
                          , field "twitter" $ return . Map.findWithDefault "" "twitter" . itemBody
                          , field "linkedin" $ return . Map.findWithDefault "" "linkedin" . itemBody
                          , field "github" $ return . Map.findWithDefault "" "github" . itemBody
                          , field "site" $ return . Map.findWithDefault "" "site" . itemBody ] )
                ( sequence [ makeItem ( Map.fromList [ ( "key", "alex")
                                                     , ( "name", "Alex Shure")
                                                     , ( "projects", "open-droplet")
                                                     , ( "affiliations", "Open Source Ecology Germany|http://opensourceecology.de/,Open it agency|http://openitagency.eu/")
                                                     , ( "description", "Alex Shure is a skilled craftsman and inventor active in the field of open source hardware within many different projects. As a human-nerd-interactor he pushes open source beyond the world of software - to infinity and beyond, starting with Open Source Ecology Germany and the Open it Agency.")
                                                     , ( "twitter", "https://twitter.com/AlexShure")
                                                     , ( "linkedin", "http://www.linkedin.com/in/jmatsushita" )
                                                     , ( "github", "https://github.com/aShure" )
                                                     , ( "site", "http://etemu.com/") ] )
                           , makeItem ( Map.fromList [ ( "key", "elf")
                                                     , ( "name", "elf Pavlik")
                                                     , ( "projects", "open-oil-framework")
                                                     , ( "affiliations", "Hackers4peace|http://hackers4peace.net/,PolyEconomy|http://polyeconomy.info/" )
                                                     , ( "description", "#hacker / #elf - living strictly #moneyless and #stateless already for over 5 years! @hackers4peace @polyeconomy #WorldPeaceGame #ZeroWaste")
                                                     , ( "twitter", "https://twitter.com/elfpavlik")
                                                     , ( "linkedin", "http://de.linkedin.com/pub/kat-austen/4/579/216" )
                                                     , ( "github", "https://github.com/elf-pavlik" )
                                                     , ( "site", "https://wwelves.org/perpetual-tripper/") ] )
                           , makeItem ( Map.fromList [ ( "key", "sam")
                                                     , ( "name", "Sam Muirhead")
                                                     , ( "projects", "open-droplet")
                                                     , ( "affiliations", "Year Of Open Source|http://yearofopensource.net,Open it agency|http://openitagency.eu/")
                                                     , ( "description", "I make all sorts of videos – but I have a focus on using, explaining and promoting Free/Libre/Open Source Software, Free Culture, Open Knowledge and Open Source Hardware. In 2012/13 I lived a Year of Open Source, I’m part of the Open It Agency, and I also run post-production workshops with free software!")
                                                     , ( "twitter", "https://twitter.com/cameralibre")
                                                     , ( "linkedin", "http://de.linkedin.com/pub/kat-austen/4/579/216" )
                                                     , ( "github", "https://github.com/samoos" )
                                                     , ( "site", "http://cameralibre.cc/") ] )
                           ])
    , listField "partners"
                ( mconcat [ field "key" $ return . partner_key . itemBody 
                          , field "name" $ return . partner_name . itemBody 
                          , field "link" $ return . partner_link . itemBody ] )
                ( mapM makeItem partners )
    , defaultContext
    ]

--------------------------------------------------------------------------------


-- Instead of the Map syntax let's try Records.

data Partner = Partner { partner_key :: String
                       , partner_name :: String
                       , partner_link :: String
                       } 

partners = [ Partner { partner_key = "atchai"
                     , partner_name = "Atchai"
                     , partner_link = "http://www.atchai.com/"
                     }
           , Partner { partner_key = "ucl"
                     , partner_name = "University College London"
                     , partner_link = "https://www.ucl.ac.uk/"
                     }
           , Partner { partner_key = "engineroom"
                     , partner_name = "The Engine Room"
                     , partner_link = "https://www.theengineroom.org/"
                     }
           , Partner { partner_key = "greenhost"
                     , partner_name = "Greenhost"
                     , partner_link = "https://greenhost.net/"
                     }
           , Partner { partner_key = "resurgence"
                     , partner_name = "Resurgence"
                     , partner_link = "http://resurgence.io"
                     }
           , Partner { partner_key = "ipl"
                     , partner_name = "Internet Protection Lab"
                     , partner_link = "http://www.internetprotectionlab.net/"
                     }
           , Partner { partner_key = "gfmd"
                     , partner_name = "Global Forum for Media Development"
                     , partner_link = "http://gfmd.info"
                     }
           ]


data Staff = Staff { key :: String
                   , name :: String
                   , offset :: Bool
                   , title :: String
                   , description :: String
                   , twitter :: String
                   , linkedin :: String
                   , github :: String
                   , site :: String
                   }

staff = [ Staff { key = "jun"
                , name = "Jun Matsushita"
                , offset = True
                , title = "CEO, Founder"
                , description = "Jun has been advising international non-profits, humanitarian organisations and media organisations, in the use of innovation and technology for more than 16 years in Paris, New York and London. His technical expertise ranges from system and network administration, web and telephony platforms, to digital security and knowledge management."
                , twitter = "https://twitter.com/jmatsushita"
                , linkedin = "http://www.linkedin.com/in/jmatsushita"
                , github = "https://github.com/jmatsushita"
                , site = "https://iilab.org"
                }
        , Staff { key = "kat"
                , name = "Kat Austen"
                , offset = False
                , title = "Head of Research and Design"
                , description = "Kat is a person. She’s interested in lots of things and phenomena, how things are connected, and why they are connected. She likes patterns but doesn’t have to have them. In the temporal melting-pot of her life so far she has been a scientist, an artist, a journalist and a writer. She welcomes a humane and environmentally kind future."
                , twitter = "https://twitter.com/katausten"
                , linkedin = "http://de.linkedin.com/pub/kat-austen/4/579/216"
                , github = "https://github.com/iamkat"
                , site = "http://katausten.com" 
                } 
        ]



--------------------------------------------------------------------------------
-- | Find the first instance of needle (must be non-empty) in haystack. We
-- return the suffix of haystack after needle is matched.
--
-- Examples:
--
-- > needleSuffix "cd" "abcde" = "e"
--
-- > needleSuffix "ab" "abc" = "c"
--
-- > needleSuffix "ab" "xxab" = ""
--
-- > needleSuffix "a" "xx" = ""
needleSuffix :: String -> String -> Maybe String
needleSuffix needle haystack = go [] haystack
  where
    go _   []                     = Nothing
    go acc xss@(x:xs)
        | needle `isPrefixOf` xss = Just $ xss
        | otherwise               = go (x : acc) xs
--------------------------------------------------------------------------------

zipPad :: (Monoid a, Monoid b) => [a] -> [b] -> [(a, b)]
zipPad xs [] = zip xs (repeat mempty)
zipPad [] ys = zip (repeat mempty) ys
zipPad (x:xs) (y:ys) = (x, y) : zipPad xs ys

--------------------------------------------------------------------------------
listField :: String -> Context a -> Compiler [Item a] -> Context b
listField key c xs = listFieldWith' key c (const xs)

--------------------------------------------------------------------------------

listFieldWith' :: String -> Context a -> (Item b -> Compiler [Item a]) -> Context b
listFieldWith' key c f = field' key $ fmap (ListField c) . f
--listFieldWith' key c _ = Context $  \k _ i -> empty
--listFieldWith' key c f = field' key $ fmap (ListField c) . (\i -> do 
--    debugCompiler $ "listFieldWith - " <> proj <> " " <> show project
--    emptyList <- fmap null $ f i
--    if emptyList then return empty else f i )

--------------------------------------------------------------------------------
field' :: String -> (Item a -> Compiler ContextField) -> Context a
field' key value = Context $ \k _ i -> if k == key 
    then do
        v <- value i
        case v of 
            ListField _ xs -> if (null xs) then empty else return v
            _ -> return v
    else empty

