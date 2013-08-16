{-# LANGUAGE OverloadedStrings #-}

module Models (
    getRanks
  , getRecent
  , getPlayerMatches
  , getMatches
  , getCurrent
  , getRank
) where

import Types

import qualified Database.Redis        as R
import qualified Data.ByteString.Char8 as C
import qualified Data.Text.Encoding    as T
import qualified Data.Aeson            as JSON
import qualified Data.ByteString.Lazy  as BL

import Data.Attoparsec.ByteString.Char8
import Control.Applicative
import Data.Maybe (mapMaybe)
import Data.Semigroup

instance Semigroup C.ByteString where
  (<>) = mappend

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

