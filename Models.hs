{-# LANGUAGE OverloadedStrings #-}

module Models (
    getPlayer
  , getRecent
  , getMatches
  , getCurrent
  , getRatings
  , subscribe
) where

import Types

import qualified Database.Redis        as R
import Database.Redis.Core
import Database.Redis.PubSub hiding(subscribe)

import qualified Data.ByteString.Char8 as C
import qualified Data.Text.Encoding    as T
import qualified Data.Aeson            as JSON
import qualified Data.ByteString.Lazy  as BL

import Data.Attoparsec.ByteString.Char8
import Control.Applicative
import Data.Maybe (mapMaybe)
import Data.Semigroup
import Control.Monad

import Control.Monad.Trans
import Data.Conduit


instance Semigroup C.ByteString where
  (<>) = mappend


---------- PubSub EventStream

-- Monad m, ToMarkup a => Conduit a m Html

subscribe :: (JSON.FromJSON a, R.MonadRedis r) => C.ByteString -> Source r a
subscribe chan = mapOutputMaybe (JSON.decode . BL.fromChunks . return) $  src chan

subscribe' :: C.ByteString -> R.Redis (Either R.Reply C.ByteString)
subscribe' channel = R.sendRequest ["SUBSCRIBE", channel]

src :: R.MonadRedis r => C.ByteString -> Source r C.ByteString
src chan = do
  liftRedis . subscribe' $ chan
  forever $ recv >>= yield . pubsubToBS . R.decodeMsg

instance R.MonadRedis r => R.MonadRedis (ConduitM i o r) where
  liftRedis = lift . liftRedis

pubsubToBS (R.Msg (R.Message _ msg)) = msg

----- Conduit ^^^^^^^^


fuck r = r >>= either
  (\(R.Error x) -> error . show $ x)
  return

fucking r = r >>= maybe
  (error "key not found")
  return

getRatings index count = do
  R.select 1
  r <- fuck $ R.zrevrangeWithscores "players" index (index + count - 1)
  let f rank (name, score) = RatingEntry (Name . T.decodeUtf8 $ name) rank (Rating score)
  return . RatingTable index .  zipWith f [index + 1 ..] $ r   

getRecent :: R.Redis RecentMatches
getRecent = RecentMatches <$> getMatches "history" 0 20

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
  mkPlayer = Name . T.decodeUtf8

getPlayer :: Name -> R.Redis Player
getPlayer (Name name) = Player (Name name)
  <$> rank
  <*> rating
  <*> matches where
    key = T.encodeUtf8 name
    rank = fucking . fuck $ R.zrevrank "players" key
    rating = Rating <$> (fucking . fuck $ R.zscore "players" key)
    matches = getPlayerMatches key

