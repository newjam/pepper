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
import           Text.Blaze.Html.Renderer.Text

import qualified Text.Markdown               as MD
import qualified Data.Text.Lazy.IO           as LT
import qualified Data.Text.Encoding          as T

-- Todo
--   * pagination/search of lists, ie history and scoreboard

main = do
  -- Thread safe connection pool
  conn <- R.connect R.defaultConnectInfo
  -- start scotty server
  scotty 8001 (routes conn)

routes conn = do
 
  get "/about" $ do
    about <- liftIO $ LT.readFile "about"
    html. renderHtml . page $ do
      MD.markdown MD.def about

  get "/favicon.png" $ do
    header "content-type" "image/png"
    file "favicon.png"

  get "/style.css" $ do
    header "content-type" "text/css"
    file "style.css"

  get "/scoreboard" $ do
    rankings <- liftIO . R.runRedis conn $ getRatings 0 30
    current <- liftIO . R.runRedis conn $ do
      R.select 1
      getCurrent
    html . renderHtml . page $ do
      H.h2 $ do "Current Match"
      H.toHtml current
      H.h2 $ do "Rankings" 
      H.toHtml rankings
  get "/ranks" $ redirect "/history" 
  get "/player/:player" $ \playerNameBS -> do
    let decoded = decodeUrl playerNameBS
    let name = Name . T.decodeUtf8 $ decoded
    player <- liftIO . R.runRedis conn $ getPlayer name
    html . renderHtml . page . H.toHtml $ player    

  get "/history" $ do
    current <- liftIO . R.runRedis conn $ do
      R.select 1
      getCurrent
    recent <- liftIO . R.runRedis conn $ getRecent

    html . renderHtml $ page $ do
      H.h2 $ do "Current Match"
      H.toHtml current
      H.h2 $ do "Recent Matches"
      H.div $ H.toHtml recent


page content = do
  H.html $ do
    H.head $ do
      H.title "Pepper"
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

