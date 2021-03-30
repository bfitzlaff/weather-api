{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Client
    ( getWeatherInfo
    ) where

import           Servant                        ( type (:>)
                                                , Get
                                                , JSON
                                                , Proxy(..)
                                                )
import           Servant.Client                 ( ClientM
                                                , client
                                                )
import           Types                          ( Latitude
                                                , Longitude
                                                , RequiredQueryParam
                                                , WeatherApiKey
                                                , WeatherInfo
                                                )
type API =
    "onecall" :> RequiredQueryParam "appid" WeatherApiKey :> RequiredQueryParam "lat" Latitude :> RequiredQueryParam "lon" Longitude :> Get '[JSON] WeatherInfo 

api :: Proxy API
api = Proxy

getWeatherInfo :: WeatherApiKey -> Latitude -> Longitude -> ClientM WeatherInfo
getWeatherInfo = client api
