{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}

module Types where

import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.List.NonEmpty             ( NonEmpty(..) )
import           Data.OpenApi                   ( ToParamSchema
                                                , ToSchema
                                                )
import           Data.Text                      ( Text )
import           Data.Typeable                  ( Typeable )
import           Dhall                          ( FromDhall )
import           GHC.Generics                   ( Generic )
import           Servant                        ( FromHttpApiData
                                                , QueryParam'
                                                , Required
                                                , Strict
                                                , ToHttpApiData
                                                )

type RequiredQueryParam = QueryParam' '[Required , Strict]

newtype Latitude = Latitude { unLatitude :: Double }
    deriving Show
    deriving newtype (FromHttpApiData, ToHttpApiData, Typeable, ToParamSchema)

newtype Longitude = Longitude { unLongitude :: Double }
    deriving Show
    deriving newtype (FromHttpApiData, ToHttpApiData, Typeable, ToParamSchema)

newtype WeatherApiKey = WeatherApiKey { unWeatherApiKey :: Text }
    deriving Show
    deriving newtype (FromHttpApiData, ToHttpApiData, FromDhall)

data Alert = Alert
    { event       :: Text
    , description :: Text
    }
    deriving (Generic, Show, Typeable)
    deriving anyclass (FromJSON, ToJSON, ToSchema)

newtype Condition = Condition { description :: Text }
    deriving (Generic, Show)
    deriving anyclass (FromJSON)

data CurrentWeather = CurrentWeather
    { temp    :: Double
    , weather :: NonEmpty Condition
    }
    deriving (Generic, Show)
    deriving anyclass FromJSON

data WeatherInfo = WeatherInfo
    { current :: CurrentWeather
    , alerts  :: [Alert]
    }
    deriving (Generic, Show)
    deriving anyclass FromJSON

data TemperatureOverview = Cold | Moderate | Hot
    deriving (Generic, Show, Typeable)
    deriving anyclass (ToJSON, ToSchema)

data WeatherResponse = WeatherResponse
    { condition :: Text
    , temp      :: TemperatureOverview
    , alerts    :: [Alert]
    }
    deriving (Generic, Show, Typeable)
    deriving anyclass (ToJSON, ToSchema)

mkWeatherResponse :: WeatherInfo -> WeatherResponse
mkWeatherResponse (WeatherInfo (CurrentWeather t ((Condition d) :| _)) a) =
    WeatherResponse d overview a
  where
    overview | t < 40    = Cold
             | t > 70    = Hot
             | otherwise = Moderate
