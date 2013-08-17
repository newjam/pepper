{-# LANGUAGE OverloadedStrings #-}

import Types
import Models

import           Web.Scotty
import qualified Database.Redis as R

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
import Data.Conduit.List as CL

import Network.Wai.EventSource.EventStream
--import Network.Wai.EventSource

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.ByteString (fromByteString)
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

  -- start scotty server
  scotty 8001 (routes conn)

routes conn = do

  get "/stream" $ do
    header "Content-Type" "text/event-stream"
    bloop <- liftIO . runResourceT $ foo conn
    source bloop
 
  get "/about" $ do
    about <- liftIO $ LT.readFile "about"
    html. renderHtml . page $ do
      MD.markdown MD.def about

  get "/pepper.js" $ do
    header "content-type" "text/javascript"
    file "static/pepper.js"

  get "/favicon.png" $ do
    header "content-type" "image/png"
    file "static/favicon.png"

  get "/style.css" $ do
    header "content-type" "text/css"
    file "static/style.css"

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


page content = do
  H.html $ do
    H.head $ do
      H.title "Pepper"
      script "/pepper.js"
      script "//cdnjs.cloudflare.com/ajax/libs/zepto/1.0rc1/zepto.min.js"
      H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "/style.css"
      H.link ! A.rel "shortcut icon" ! A.type_ "image/png" ! A.href "/favicon.png"
    H.body $ do
      H.header $ do
        H.span ! A.class_ "title" $ do "Pepper"
        H.nav $ do
          H.ul $ do
            H.li $ H.a ! A.href "/scoreboard" $ "scoreboard"
            H.li $ H.a ! A.href "/history" $ "history"
            H.li $ H.a ! A.href "/about" $ "about"
      H.section $ content

