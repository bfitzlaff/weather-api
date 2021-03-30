{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Config
    ( AppConfig(..)
    , loadConfig
    ) where

import           Dhall                          ( FromDhall
                                                , auto
                                                , input
                                                )
import           GHC.Generics                   ( Generic )
import           GHC.Natural                    ( Natural )
import           Types                          ( WeatherApiKey )

data AppConfig = AppConfig
    { appPort :: Natural
    , apiKey  :: WeatherApiKey
    }
    deriving (Generic, FromDhall, Show)

loadConfig :: IO AppConfig
loadConfig = input auto "./config/settings.dhall"
