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

search word (NameIndex i) = process . findAll (V.fromList word) $ i
  where
    findAll = Adict.findAll Adict.costDefault 2.0

process :: [(String, String, Double)] -> [Name]
process = map stringToName . L.nub . map (\(_, x, _) -> x) . sortWith (\(_, _, x) -> x) 

nameToString (Name n) = T.unpack n 
stringToName = Name . T.pack 

newtype NameIndex = NameIndex (S.DAWG Char () String)

index :: [Name] -> NameIndex 
index = NameIndex . S.freeze . foldr addWord D.empty . map nameToString

type Auto a = a -> a

addWord :: String -> Auto (D.DAWG Char String)
addWord word dawg = foldr (\ngram -> D.insert ngram word) dawg . ngrams $ word

ngrams = filter ((>=2) . length) . concatMap L.inits . L.tails . map toLower

