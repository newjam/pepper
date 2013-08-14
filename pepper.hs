{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


import Web.Scotty
import qualified Database.Redis as R
import Control.Monad.IO.Class (liftIO)

import Text.Blaze as B
import qualified Text.Blaze.Html5 as H --hiding (html, param)
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Internal (textValue)

import qualified Text.Markdown as MD

import Text.Blaze.Html.Renderer.Text

import qualified Data.ByteString.Char8 as C

import qualified Data.Text as T
import Data.Text.Encoding as T

import qualified Data.Text.Lazy.IO as LT

import Data.Attoparsec.ByteString.Char8

import Control.Monad

import Data.Aeson as JSON hiding (json)
import Control.Applicative

import Data.Maybe (mapMaybe)

import qualified Data.ByteString.Lazy as BL

import Data.Semigroup

import Web.Encodings (encodeUrl, decodeUrl)

instance Semigroup C.ByteString where
  (<>) = mappend
instance Semigroup T.Text where
  (<>) = mappend

-- Todo
--   * pagination/search of lists, ie history and scoreboard
--   * modularize the code so we don't have one big file

main = do
  -- Thread safe connection pool
  conn <- R.connect R.defaultConnectInfo
  -- start scotty server
  scotty 8001 (routes conn)

-- Types
-- =====

data PlayerRank = PlayerRank Player Integer Rating

data RankBoard = RankBoard Integer [PlayerRank]

newtype Rating = Rating Double
  deriving (Show, Num, Real, Ord, Eq, RealFrac, Fractional)

newtype Odds = Odds Double
  deriving (Show, Num, Real, Ord, Eq, RealFrac, Fractional)

data MatchOutcome = MatchOutcome Player Player Bool Odds
  deriving Show

data CurrentMatch = CurrentMatch Player Player Odds
  deriving Show

data MatchList = MatchList [MatchOutcome]
  deriving Show

data Player = Player T.Text
  deriving Show

instance FromJSON Player where
  parseJSON (String name) = return . Player $ name

instance FromJSON Odds where
  parseJSON (Number (D x)) = return . Odds $ x

instance FromJSON MatchOutcome where
  parseJSON (Object v) = MatchOutcome <$>
                         v .: "p1" <*>
                         v .: "p2" <*>
                         v .: "p1Won" <*>
                         v .: "odds"

sigfig :: Double -> Double
sigfig x = fromIntegral (round (x * 100.0)) / 100.0

instance ToMarkup Player where
  toMarkup (Player name) = H.a
    ! A.class_ "player"
    ! A.href (textValue $ "/player/"<>encodeUrl name)
    $ H.toHtml name

instance ToMarkup PlayerRank where
  toMarkup (PlayerRank name rank score) = do
    H.tr $ do
      H.td $ H.toHtml $ rank
      H.td $ H.toHtml $ name
      H.td $ H.toHtml $ score

instance ToMarkup RankBoard where
  toMarkup (RankBoard index ranks) = do
    H.table ! A.id "rankings" $ do
      H.thead $ H.tr $ do
        H.td $ H.toHtml $ ("rank" :: T.Text)
        H.td $ H.toHtml $ ("name" :: T.Text)
        H.td $ H.toHtml $ ("score" :: T.Text)
      forM_ ranks toMarkup

instance ToMarkup MatchList where
  toMarkup (MatchList ms) = H.table $ do
    H.thead $ do
      H.tr $ do
        H.td ! A.class_ "p1" $ do "Player 1"
        H.td ! A.class_ "p2" $ do "Player 2"
        H.td $ do "Odds"
    forM_ ms H.toHtml

instance ToMarkup Rating where
  toMarkup (Rating r) = do
    H.toHtml $ sigfig r

instance ToMarkup Odds where
  toMarkup (Odds o) = do
    H.toHtml . sigfig . (*100.0) $ o
    "%"

instance ToMarkup MatchOutcome where
  toMarkup (MatchOutcome p1 p2 p1Won odds) = do
    H.tr $ do
      let (p1Class, p2Class) = if p1Won
          then ("winner", "loser")
          else ("loser", "winner")
      H.td ! A.class_ p1Class $ H.toHtml p1
      H.td ! A.class_ p2Class $ H.toHtml p2
      let oddsClass = if odds >= 0.6
          then "correct"
          else if odds <= 0.4
            then "incorrect"
            else "ok"
      H.td ! A.class_ oddsClass $ H.toHtml odds

instance ToMarkup CurrentMatch where
  toMarkup (CurrentMatch p1 p2 odds) = do 
    H.div $ do
      H.span ! A.class_ "p1" $ H.toHtml p1
      " vs. "
      H.span ! A.class_ "p2" $ H.toHtml p2
    H.div $ do
      H.toHtml odds
      " and "
      H.toHtml (1.0 - odds)
 
-- Redis
-- =====

fuck r = r >>= either
  (\(R.Error x) -> error . show $ x)
  return

fucking r = r >>= maybe
  (error "you thought you didn't fuck up but you did.")
  return

getRanks index count = do
  R.select 1
  r <- fuck $ R.zrevrangeWithscores "players" index (index + count - 1)
  let f rank (name, score) = PlayerRank (Player . T.decodeUtf8 $ name) rank (Rating score)
  return . RankBoard index .  zipWith f [index + 1 ..] $ r   

getRecent :: R.Redis MatchList -- [MatchOutcome]
getRecent = getMatches "history" 0 20

getPlayerMatches :: C.ByteString -> R.Redis MatchList
getPlayerMatches player = getMatches key 0 20 where
  key = "games["<>player<>"]"

getMatches :: C.ByteString -> Integer -> Integer -> R.Redis MatchList
getMatches key start count = do
  R.select 1
  bs <- fuck $ R.lrange key start (start + count - 1)
  return . MatchList . mapMaybe (JSON.decode . BL.fromChunks . return) $ bs

getCurrent :: R.Redis CurrentMatch
getCurrent = CurrentMatch <$> p1 <*> p2 <*> odds where
  p1 = mkPlayer <$> get "p1"
  p2 = mkPlayer <$> get "p2"
  odds = Odds . parseDouble <$> get "odds"
  parseDouble = either (error) id . parseOnly double
  get key = fucking . fuck $ R.get key
  mkPlayer = Player . T.decodeUtf8


getRank :: Player -> R.Redis PlayerRank
getRank (Player name) = PlayerRank (Player name) <$> rank <*> rating where
  key = T.encodeUtf8 name
  rank = fucking . fuck $ R.zrank "players" key
  rating = Rating <$> (fucking . fuck $ R.zscore "players" key)


-- controller

routes conn = do
 
  get "/about" $ do
    about <- liftIO $ LT.readFile "about"
    html. renderHtml . page $ do
      MD.markdown MD.def about

  get "/style.css" $ do
    header "content-type" "text/css"
    file "style.css"

  get "/scoreboard" $ do
    rankings <- liftIO . R.runRedis conn $ getRanks 0 30
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
    let player = Player . T.decodeUtf8 $ decoded
    matches <- liftIO . R.runRedis conn $ getPlayerMatches decoded
    PlayerRank _ _ rating <- liftIO . R.runRedis conn $ getRank player
    
    html . renderHtml . page $ do
      H.h2 $ do
        H.toHtml player
        "'s Matches"
      H.h4 $ do
        "Elo rating of "
        H.toHtml rating
      toMarkup matches
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
    H.body $ do
      H.header $ do
        H.span ! A.class_ "title" $ do "Pepper"
        H.nav $ do
          H.ul $ do
            H.li $ H.a ! A.href "/scoreboard" $ "scoreboard"
            H.li $ H.a ! A.href "/history" $ "history"
            H.li $ H.a ! A.href "/about" $ "about"
      H.section $ content

