--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative ((<$>))
import           Data.Monoid         (Monoid, mempty, mconcat, (<>))
import           Data.Map        (Map)
import qualified Data.Map     as  M
import           Hakyll
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Text as T
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
    match ("_assets/js/ie/*" .||. "_assets/js/*.map") $ do
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
            isotope <- load "_assets/js/jquery.isotope.min.js"
            easytabs <- load "_assets/js/jquery.easytabs.min.js"
            viewport <- load "_assets/js/viewport-units-buggyfill.js"            
            scripts <- load "_assets/js/theme_scripts.js"
            iilab <- load "_assets/js/iilab.js"            
--            concatItems (core:items) >>= withItemBody compressJsCompiler
            makeItem $ unlines (map itemBody (((jquery:easing:bootstrap:hover:skrollr:skrollrstyle:waypoints:waypointssticky:carousel:isotope:easytabs:viewport:scripts:iilab:[])) :: [Item String]))

{--    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "_templates/default.html" defaultContext
            >>= relativizeUrls
--}
    match "_posts/blog/*" $ do
        route $ setExtension "html" `composeRoutes` (gsubRoute "_posts/" (const "")) 
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "_templates/blog/content.html"    postCtx
            >>= saveSnapshot "blog-content"
            >>= loadAndApplyTemplate "_templates/blog/post.html"    postCtx
            >>= loadAndApplyTemplate "_templates/default.html" postCtx
            >>= relativizeUrls

    match "_posts/projects/*" $ do
        route $ setExtension "html" `composeRoutes` (gsubRoute "_posts/" (const ""))
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "_templates/project.html"    projectCtx
            >>= saveSnapshot "project-content"
            >>= loadAndApplyTemplate "_templates/default.html" projectCtx
            >>= relativizeUrls


    create ["index.html"] $ do
        route idRoute
        compile $ do
            posts <- fmap (take 10) . recentFirst =<<
                loadAllSnapshots "_posts/blog/*" "blog-content"
            projects <- fmap (take 10) . recentFirst =<<
                loadAllSnapshots "_posts/projects/*" "project-content"
            let indexCtx =
                    listField "posts" postCtx (return posts) <>
                    listField "projects" postCtx (return projects) <>
                    constField "title" "Innovative Technology for Social Impact" <>
                    siteCtx <>
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "_templates/index.html" indexCtx
                >>= loadAndApplyTemplate "_templates/default.html" indexCtx
                >>= relativizeUrls

    -- Render blog index feed
    create ["blog.html"] $ do
        route idRoute
        compile $ do
            posts <- fmap (take 10) . recentFirst =<<
                loadAllSnapshots "_posts/blog/*" "blog-content"
            let ctx =
                    listField "posts" postCtx (return posts) <>
                    constField "title" "Blog" <>
                    siteCtx <>
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "_templates/blog.html" ctx
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
    teaserField "teaser" "blog-content" <>
    testCtx <>
    formatCtx <>
    iconCtx <>
    postImagesCtx <>
    siteCtx <>
    defaultContext


--------------------------------------------------------------------------------
testCtx :: Context a
testCtx = functionField "test" $  \x _ -> case x of
                                  _     -> return "test empty!"

--------------------------------------------------------------------------------
formatCtx :: Context String
formatCtx = field "blog" ( getFormat (== "blog") ) <>
            field "social" ( getFormat (== "social") ) <> 
            field "link" ( getFormat (== "link") ) <> 
            field "tech" ( getFormat (== "tech") ) 

getFormat :: (String -> Bool) -> Item String -> Compiler String
getFormat f item = do 
    format <- getMetadataField' (itemIdentifier item) "format"
    return (if f format then "true" else "")

--------------------------------------------------------------------------------
iconCtx :: Context a
iconCtx = field "icon-format" $ \item -> do
    format <- getMetadataField (itemIdentifier item) "format"
    -- Inspect metadata and decide title based on that
    case format of 
        Just "blog" -> return "edit"
        Just "social" -> return "quote"
        Just "link" -> return "link"
        Just "tech" -> return "tools"
        Just "format" -> return "wat?"
        Nothing -> return "edit"

--------------------------------------------------------------------------------
postImagesCtx :: Context String
postImagesCtx = listFieldWith "images" (imageCtx) getImages

getImages :: Item String -> Compiler [Item (String,String)]
getImages item = do
        src <- getMetadataField' (itemIdentifier item) "images"
        caption <- getMetadataField' (itemIdentifier item) "images-caption"
        srcs <- return $ map (trim . T.unpack) $ T.splitOn "," ( T.pack src )
        captions <- return $ map (trim . T.unpack) $ T.splitOn "|" ( T.pack caption )
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
    , constField "site.contact.address2" "iilab Berlin<br> Thinkfarm Berlin, Oranienstraße 183, Aufgang C, 3. OG<br>10999 Berlin - Germany"
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
    , constField "programs.access" "Access"
    , constField "programs.security" "Security"
    , constField "programs.decision" "Decision Support"
    , constField "programs.collaboration" "Collaboration"
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
    , constField "persons.jun.name" "Jun Matsushita"
    , constField "persons.jun.title" "CEO, Founder"
    , constField "persons.jun.description" "Jun has been advising international non-profits, humanitarian organisations and media organisations, in the use of innovation and technology for more than 16 years in Paris, New York and London. His technical expertise ranges from system and network administration, web and telephony platforms, to digital security and knowledge management."
    , constField "persons.jun.twitter" "https://twitter.com/jmatsushita"
    , constField "persons.jun.linkedin" "http://www.linkedin.com/in/jmatsushita"
    , constField "persons.jun.github" "https://github.com/jmatsushita"
    , constField "persons.jun.site" "https://iilab.org"
    , constField "persons.kat.name" "Kat Austen"
    , constField "persons.kat.title" "Head of Research and Design"
    , constField "persons.kat.description" "Kat is a person. She’s interested in lots of things and phenomena, how things are connected, and why they are connected. She likes patterns but doesn’t have to have them. In the temporal melting-pot of her life so far she has been a scientist, an artist, a journalist and a writer. She welcomes a humane and environmentally kind future."
    , constField "persons.kat.twitter" "https://twitter.com/katausten"
    , constField "persons.kat.linkedin" "http://de.linkedin.com/pub/kat-austen/4/579/216"
    , constField "persons.kat.github" "https://github.com/iamkat"
    , constField "persons.kat.site" "http://katausten.com"
    , constField "person_contributors.alex.name" "Alex Shure"
    , constField "person_contributors.alex.description" "Alex Shure is a skilled craftsman and inventor active in the field of open source hardware within many different projects. As a human-nerd-interactor he pushes open source beyond the world of software - to infinity and beyond, starting with Open Source Ecology Germany and the Open it Agency."
    , constField "person_contributors.alex.site" "http://etemu.com/"
    , constField "person_contributors.alex.twitter" "https://twitter.com/AlexShure"
    , constField "person_contributors.alex.github" "https://github.com/aShure"
    , constField "person_contributors.alex.projects" "open-droplet"
    , constField "person_contributors.alex.affiliations" "Open Source Ecology Germany|http://opensourceecology.de/,Open it agency|http://openitagency.eu/"
    , constField "person_contributors.elf.name" "elf Pavlik"
    , constField "person_contributors.elf.description" "#hacker / #elf - living strictly #moneyless and #stateless already for over 5 years! @hackers4peace @polyeconomy #WorldPeaceGame #ZeroWaste"
    , constField "person_contributors.elf.site" "http://etemu.com/"
    , constField "person_contributors.elf.twitter" "https://twitter.com/AlexShure"
    , constField "person_contributors.elf.github" "https://github.com/elf-pavlik"
    , constField "person_contributors.elf.projects" "open-oil-framework"
    , constField "person_contributors.elf.affiliations" "Hackers4peace|http://hackers4peace.net/,PolyEconomy|http://polyeconomy.info/"
    , constField "person_contributors.sam.name" "Sam Muirhead"
    , constField "person_contributors.sam.twitter" "https://twitter.com/cameralibre"
    , constField "person_contributors.sam.github" "https://github.com/samoos"
    , constField "person_contributors.sam.description" "I make all sorts of videos – but I have a focus on using, explaining and promoting Free/Libre/Open Source Software, Free Culture, Open Knowledge and Open Source Hardware. In 2012/13 I lived a Year of Open Source, I’m part of the Open It Agency, and I also run post-production workshops with free software!"
    , constField "person_contributors.sam.site" "http://cameralibre.cc/"
    , constField "person_contributors.sam.projects" "open-droplet"
    , constField "person_contributors.sam.affiliations" "Year Of Open Source|http://yearofopensource.net,Open it agency|http://openitagency.eu/"
    , constField "partners.atchai.name" "Atchai"
    , constField "partners.atchai.link" "http://www.atchai.com/"
    , constField "partners.engineroom.name" "The Engine Room"
    , constField "partners.engineroom.link" "https://www.theengineroom.org/"
    , constField "partners.greenhost.name" "Greenhost"
    , constField "partners.greenhost.link" "https://greenhost.net/"
    , constField "partners.resurgence.name" "Resurgence"
    , constField "partners.resurgence.link" "http://resurgence.io"
    , constField "partners.ipl.name" "Internet Protection Lab"
    , constField "partners.ipl.link" "http://www.internetprotectionlab.net/"
    , constField "partners.gfmd.name" "Global Forum for Media Development"
    , constField "partners.gfmd.link" "http://gfmd.info"
    , defaultContext
    ]

--------------------------------------------------------------------------------

zipPad :: (Monoid a, Monoid b) => [a] -> [b] -> [(a, b)]
zipPad xs [] = zip xs (repeat mempty)
zipPad [] ys = zip (repeat mempty) ys
zipPad (x:xs) (y:ys) = (x, y) : zipPad xs ys