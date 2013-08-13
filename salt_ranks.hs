{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


import Web.Scotty
import qualified Database.Redis as R
import Control.Monad.IO.Class (liftIO)

import Text.Blaze as B
import qualified Text.Blaze.Html5 as H --hiding (html, param)
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Internal (textValue)

--import qualified Text.Pandoc.Readers.Mardown as Pandoc
--import qualified Text.Pandoc.Writers.HTML as Pandoc

import qualified Text.Markdown as MD

import Text.Blaze.Html.Renderer.Text

import qualified Data.ByteString.Char8 as C

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text.Encoding as TE

import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT

import Data.Attoparsec.ByteString.Char8

import Control.Monad

--import qualified Data.Aeson as JSON
import Data.Aeson as JSON hiding (json)
import Control.Applicative

import Data.Maybe (mapMaybe)

import qualified Data.ByteString.Lazy as BL

import Data.String

import Data.Semigroup

instance Semigroup C.ByteString where
  (<>) = mappend
instance Semigroup T.Text where
  (<>) = mappend

-- Todo
--   * pagination of lists, ie history and scoreboard
--   * character profile displaying recent games and detailed statistics.
--   *

main = scotty 8001 routes

fuck r = r >>= either
  (\(R.Error x) -> error . show $ x)
  return

fucking r = r >>= maybe
  (error "you thought you didn't fuck up but you did.")
  return

data PlayerRank = PlayerRank Player Integer Rating
data RankBoard = RankBoard Integer [PlayerRank]

newtype Rating = Rating Double
  deriving (Show, Num, Real, Ord, Eq, RealFrac, Fractional)

newtype Odds = Odds Double
  deriving (Show, Num, Real, Ord, Eq, RealFrac, Fractional)

data MatchOutcome = MatchOutcome Player Player Bool Odds
  deriving Show

data MatchList = MatchList [MatchOutcome]
  deriving Show

data Player = Player {playerName::T.Text}
  deriving Show

instance ToMarkup Player where
  toMarkup (Player name) = H.a ! A.href (textValue $ "/player/"<>name) $ H.toHtml name

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
{-
instance ToJSON MatchOutcome where
  toJSON (MatchOutcome (Player p1) (Player p2) p1Won odds) =
      object ["p1" .= p1, "p2" .= p2, "p1Won" .= p1Won, "odds" .= odds]
-}

--bleh (name, score) = PlayerRank name score

boo :: Double -> Double
boo x = fromIntegral (round (x * 100.0)) / 100.0

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
        H.td $ do "Player 1"
        H.td $ do "Player 2"
        H.td $ do "Odds"
    forM_ ms H.toHtml

instance ToMarkup Rating where
  toMarkup (Rating r) = do
    H.toHtml $ boo $ r

instance ToMarkup Odds where
  toMarkup (Odds o) = do
    H.toHtml . boo . (*100.0) $ o
    "%"

instance ToMarkup MatchOutcome where
  toMarkup (MatchOutcome p1 p2 p1Won odds) = do
    H.tr $ do
      let (p1Class, p2Class) = if p1Won
          then ("winner", "loser")
          else ("loser", "winner")
      H.td ! A.class_ p1Class $ H.toHtml p1
      H.td ! A.class_ p2Class $ H.toHtml p2
      H.td $ H.toHtml odds

getRanks index count = do
  R.select 1
  r <- fuck $ R.zrevrangeWithscores "players" index (index + count - 1)
  let f rank (name, score) = PlayerRank (Player . TE.decodeUtf8 $ name) rank (Rating score)
  return . RankBoard index .  zipWith f [index + 1 ..] $ r   

getRecent :: R.Redis MatchList -- [MatchOutcome]
getRecent = getMatches "history" 0 20

getPlayerMatches :: C.ByteString -> R.Redis MatchList
getPlayerMatches player = getMatches key 0 10 where
  key = "games["<>player<>"]"

getMatches :: C.ByteString -> Integer -> Integer -> R.Redis MatchList
getMatches key start count = do
  R.select 1
  bs <- fuck $ R.lrange key start (start + count - 1)
  return . MatchList . mapMaybe (JSON.decode . BL.fromChunks . return) $ bs

getCurrent :: R.Redis (Odds, T.Text, T.Text) --C.ByteString, C.ByteString)
getCurrent = do
  R.select 1
  x <- fucking . fuck $ R.get "p1"
  y <- fucking . fuck $ R.get "p2"
  odds <- fucking . fuck $ R.get "odds"
  let parseDouble = either (error) id . parseOnly double
  return (Odds . parseDouble $ odds, TE.decodeUtf8 x, TE.decodeUtf8 y)

getPlayerRank :: Player -> R.Redis PlayerRank
getPlayerRank (Player name) = PlayerRank (Player name) <$> rank <*> rating where
  key = TE.encodeUtf8 name
  rank = fucking . fuck $ R.zrank "players" key
  rating = Rating <$> (fucking . fuck $ R.zscore "players" key)

routes = do
  -- Thread safe connection pool
  conn <- liftIO $ R.connect R.defaultConnectInfo
 
  get "/about" $ do
    about <- liftIO $ LT.readFile "about"
    html. renderHtml . page $ do
      --H.h2 $ do "About"
      MD.markdown MD.def about -- "Header\n======\n\nI love cock!"  -- about

  get "/style.css" $ do
    header "content-type" "text/css"
    file "style.css"
  get "/scoreboard" $ do
    rankings <- liftIO . R.runRedis conn $ getRanks 0 30
    (odds, p1, p2) <- liftIO . R.runRedis conn $ getCurrent
    html . renderHtml . page $ do
      H.h2 $ do "Current Match"
      currentMatch (Player p1) (Player p2) odds
      H.h2 $ do "Rankings" 
      H.toHtml rankings
  get "/ranks" $ redirect "/history" 
  get "/player/:player" $ \playerNameBS -> do
    let player = Player . TE.decodeUtf8 $ playerNameBS
    matches <- liftIO . R.runRedis conn $ getPlayerMatches playerNameBS
    PlayerRank _ _ rating <- liftIO . R.runRedis conn $ getPlayerRank player
    
    html . renderHtml . page $ do
      H.h2 $ H.toHtml player `mappend` "'s Matches"
      --"Rank "
      --H.toHtml i
      H.h4 $ do
        "Elo rating of "
        H.toHtml rating
      toMarkup matches
  get "/history" $ do
    rankings <- liftIO . R.runRedis conn $ getRanks 0 30
    (odds, p1, p2) <- liftIO . R.runRedis conn $ getCurrent
    recent <- liftIO . R.runRedis conn $ getRecent

    html . renderHtml $ page $ do
      H.h2 $ do "Current Match"
      currentMatch (Player p1) (Player p2) odds  
      H.h2 $ do "History"
      H.div $ H.toHtml recent
--        H.h2 $ do "Rankings"
--        H.div $ toMarkup rankings

page content = do
  H.html $ do
    H.head $ do
      H.title "Pepper"
      H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "/style.css"
    H.body $ do
      H.div $ do
        H.a ! A.href "/scoreboard" $ "scoreboard"
        " | "
        H.a ! A.href "/history" $ "history"
      content


currentMatch p1 p2 odds = do
  H.div $ do 
    H.span ! A.class_ "p1" $ H.toHtml p1
    " vs. "
    H.span ! A.class_ "p2" $ H.toHtml p2
  H.div $ do
    H.toHtml odds
    " and "
    H.toHtml (1.0 - odds)
    


