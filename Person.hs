{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Person where

import GHC.Generics
import Data.Aeson
import Data.Text as T

data Person = Person {
    name :: T.Text
  , address :: T.Text
  , city :: T.Text
  , state :: T.Text
  , zip :: T.Text
  , country :: T.Text
  , telephone :: T.Text
  } deriving (Generic, Show)

cityState :: Person -> T.Text
cityState p = T.pack $ (T.unpack (Person.city p)) ++ ", " ++ (T.unpack (Person.state p))

addressFields :: Person -> [T.Text]
addressFields p = fmap (\f -> f p) [ 
  Person.address, 
  Person.cityState,
  Person.zip,
  Person.country
  ]

instance ToJSON Person where
  -- No need to provide a toJSON implementation.

  -- For efficiency, we write a simple toEncoding implementation, as
  -- the default version uses toJSON.
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Person
  -- No need to provide a parseJSON implementation.