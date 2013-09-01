{-# LANGUAGE OverloadedStrings #-}

module Index (
    index
  , search
  , NameIndex
) where

import Types

import qualified Data.DAWG.Dynamic as D
import qualified Data.DAWG.Static as S
import qualified NLP.Adict as Adict

import qualified Data.Vector as V

import qualified Data.List as L

import qualified Data.Text as T

import Data.Char (toLower)

import GHC.Exts (sortWith)

--testDawg = S.freeze . foldr addWord D.empty $ ["ryui", "ryu"]


--testQuery = V.fromList  "ryu"

-- ngrams are being overwritten...
-- we need a list as the value in the dawg, with all the names
-- it matches up to. use fromListWith

--testquery = findAll 

search query (NameIndex i) = process . findAll query' $ i
  where
    query' = V.fromList . map toLower $ query

findAll = Adict.findAll Adict.costDefault 2.0

process :: [(String, [String], Double)] -> [Name]
process = toNames . removeDuplicates . concat . extractNames . sort
  where
    toNames = map stringToName
    removeDuplicates = L.nub
    extractNames = map (\(_, name, _) -> name)
    sort = sortWith (\(_, _, distance) -> distance) 

nameToString (Name n) = T.unpack n 
stringToName = Name . T.pack 

newtype NameIndex = NameIndex (S.DAWG Char () [String])

index :: [Name] -> NameIndex 
index = NameIndex . S.fromListWith (++) . concatMap (explodeWord . nameToString)

--NameIndex . S.freeze . foldr addWord D.empty . map nameToString

--type Auto a = a -> a

--addWord :: String -> Auto (D.DAWG Char String)
--addWord word dawg = foldr (\ngram -> D.insert ngram word) dawg . ngrams $ word



explodeWord word = map (\ngram-> (ngram, [word])) . ngrams $ word

ngrams = filter ((>=2) . length) . concatMap L.inits . L.tails . map toLower

