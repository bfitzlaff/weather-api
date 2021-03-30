{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedStrings #-}

module Server
  ( startApp
  , weatherApi
  ) where

import           Client                         ( getWeatherInfo )
import           Config                         ( AppConfig(..)
                                                , loadConfig
                                                )
import           Control.Exception              ( throwIO )
import           Control.Lens                   ( (&)
                                                , (.~)
                                                , (?~)
                                                )
import           Control.Monad.IO.Class         ( liftIO )
import           Data.OpenApi                   ( OpenApi
                                                , description
                                                , info
                                                , title
                                                , version
                                                )
import           Data.Text                      ( Text )
import           Network.HTTP.Client.TLS        ( getGlobalManager )
import           Network.Wai.Handler.Warp       ( run )
import           Servant                        ( type (:<|>)(..)
                                                , type (:>)
                                                , Get
                                                , JSON
                                                , PlainText
                                                , Proxy(..)
                                                , Server
                                                , serve
                                                )
import           Servant.Client                 ( BaseUrl(BaseUrl)
                                                , ClientEnv
                                                , Scheme(Https)
                                                , mkClientEnv
                                                , runClientM
                                                )
import           Servant.OpenApi                ( HasOpenApi(toOpenApi) )
import           Types                          ( Latitude
                                                , Longitude
                                                , RequiredQueryParam
                                                , WeatherApiKey
                                                , WeatherResponse
                                                , mkWeatherResponse
                                                )

type WeatherAPI =
  "ping" :> Get '[PlainText] Text :<|>
  "weather" :> RequiredQueryParam "lat" Latitude :> RequiredQueryParam "lon" Longitude :> Get '[JSON] WeatherResponse

weatherApi :: Proxy WeatherAPI
weatherApi = Proxy

type SwaggerAPI = "openapi.json" :> Get '[JSON] OpenApi

type API = SwaggerAPI :<|> WeatherAPI

api :: Proxy API
api = Proxy

weatherOpenApi :: OpenApi
weatherOpenApi =
  toOpenApi weatherApi
    &  info
    .  title
    .~ "Weather API"
    &  info
    .  version
    .~ "1.0"
    &  info
    .  description
    ?~ "This is a simple API to get weather info by latitude and longitude"

server :: ClientEnv -> WeatherApiKey -> Server API
server c k = openApi :<|> ping :<|> getWeather
 where
  ping    = pure "pong"
  openApi = pure weatherOpenApi
  getWeather lat lon =
    liftIO $ runClientM (getWeatherInfo k lat lon) c >>= either
      throwIO
      (pure . mkWeatherResponse)

startApp :: IO ()
startApp = do
  config <- loadConfig
  let port = fromIntegral $ appPort config
  let key  = apiKey config
  clientManager <- getGlobalManager
  let baseUrl   = BaseUrl Https "api.openweathermap.org" 443 "/data/2.5"
  let clientEnv = mkClientEnv clientManager baseUrl
  run port $ serve api $ server clientEnv key
