{-# LANGUAGE OverloadedStrings #-}

import Types
import Models
import Index

import           Web.Scotty
import qualified Database.Redis as R

import qualified Network.Wai.Middleware.Static as Static
import qualified Network.Wai.Middleware.RequestLogger as RequestLogger

import           Control.Monad.IO.Class (liftIO)
import           Web.Encodings (decodeUrl)

import           Text.Blaze ((!))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Html.Renderer.Text (renderHtml)

import           Text.Blaze.Html.Renderer.Utf8 (renderHtmlBuilder)

import qualified Text.Markdown               as MD
import qualified Data.Text.Lazy.IO           as LT
import qualified Data.Text.Encoding          as T

import Data.Conduit.TMChan

import Data.Conduit
import qualified Data.Conduit.List as CL

import Network.Wai.EventSource.EventStream

import Blaze.ByteString.Builder
import qualified Data.ByteString as B

toServerEvent :: B.ByteString -> (Monad m, H.ToMarkup a) => Conduit a m ServerEvent
toServerEvent name = CL.map (serverEvent . renderHtmlBuilder . H.toHtml) where
  serverEvent = ServerEvent (Just . fromByteString $ name)  Nothing . return
  
toBuilder :: Monad m => Conduit ServerEvent m (Flush Builder)
toBuilder = CL.mapMaybe eventToBuilder =$= CL.concatMap flush where
  flush x = [Chunk x, Flush]

--foo :: R.Connection -> Source (ResourceT IO) (Flush Builder)
foo conn = outcomes >=< news  where
  toIO = transPipe (liftIO . R.runRedis conn)

  matchOutcomes ::  R.MonadRedis m => Source m MatchOutcome
  matchOutcomes = subscribe "game_over"

  newMatches :: R.MonadRedis m => Source m CurrentMatch
  newMatches = subscribe "start_game"

  outcomes = toIO $ matchOutcomes $= toServerEvent "game_over" $= toBuilder
  news = toIO $ newMatches $= toServerEvent "start_game" $= toBuilder

main = do
  -- Thread safe connection pool
  conn <- R.connect R.defaultConnectInfo

  -- Create index of player names for search
  names <- R.runRedis conn $ getNames
  let nameIndex = index names

  -- start scotty server
  scotty 8001 (routes conn nameIndex)

routes conn nameIndex = do

  -- automatically serve files from the static directory.
  -- eg GET /style.css serves static/style.css
  middleware . Static.staticPolicy $ Static.addBase "static"

  -- log information about requests to stdout
  middleware $ RequestLogger.logStdout

{-  get "/stream" $ do
    header "Content-Type" "text/event-stream"
    bloop <- liftIO . runResourceT $ foo conn
    source bloop
 -}
  get "/search" $ do
    query <- param "query"
    json . take 10 $ search query nameIndex 

  get "/about" $ do
    about <- liftIO $ LT.readFile "about"
    html. renderHtml . page $ do
      MD.markdown MD.def about

  get "/scoreboard" $ do
    rankings <- liftIO . R.runRedis conn $ getRatings 0 30
    current <- liftIO . R.runRedis conn $ do
      R.select 1
      getCurrent
    html . renderHtml . page $ do
      H.h2 $ do "Current Match"
      H.div ! A.id "currentMatch" $ H.toHtml current
      H.h2 $ do "Rankings" 
      H.toHtml rankings
  get "/ranks" $ redirect "/history" 
  get "/player/:player" $ \playerNameBS -> do
    let decoded = decodeUrl playerNameBS
    let name = Name . T.decodeUtf8 $ decoded
    player <- liftIO . R.runRedis conn $ do
      R.select 1
      getPlayer name
    html . renderHtml . page . H.toHtml $ player    

  get "/history" $ do
    current <- liftIO . R.runRedis conn $ do
      R.select 1
      getCurrent
    recent <- liftIO . R.runRedis conn $ getRecent

    html . renderHtml $ page $ do
      H.h2 $ do "Current Match"
      H.div ! A.id "currentMatch" $ H.toHtml current
      H.h2 $ do "Recent Matches"
      H.div $ H.toHtml recent


script url = H.script ! A.type_ "text/javascript" ! A.src url $ do ""
css url = H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href url

page content = do
  H.html $ do
    H.head $ do
      H.title "iPepper"
      css "/style.css"
      css "//netdna.bootstrapcdn.com/twitter-bootstrap/2.3.2/css/bootstrap-combined.min.css"
--      script "/pepper.js"
      H.link ! A.rel "shortcut icon" ! A.type_ "image/png" ! A.href "/favicon.png"
    H.body $ do
      H.header $ do
        H.div ! A.class_ "navbar navbar-inverse navbar-fixed-top"  $ do
          H.div ! A.class_ "navbar-inner" $ do
            H.div ! A.class_ "container" $ do
              H.span ! A.class_ "brand" $ do "Pepper"
              H.ul ! A.class_ "nav" $ do
                H.li $ H.a ! A.href "/scoreboard" $ "scoreboard"
                H.li $ H.a ! A.href "/history" $ "history"
                H.li $ H.a ! A.href "/about" $ "about"
              H.form ! A.class_ "navbar-search pull-right" $ do
                H.input 
                  ! H.dataAttribute "provide" "typeahead"
                  ! A.class_ "typeahead search-query"
                  ! A.placeholder "search for character"
      H.div ! A.class_ "container" $ content 
      script "http://code.jquery.com/jquery-1.8.3.min.js"
      
      script "//netdna.bootstrapcdn.com/twitter-bootstrap/2.3.2/js/bootstrap.min.js"
      script "/ajax_search.js"


