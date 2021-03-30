module ServerSpec where

import           Data.Text                      ( Text
                                                , pack
                                                )
import           Servant.OpenApi                ( validateEveryToJSON )
import           Server                         ( weatherApi )
import           Test.Hspec                     ( Spec
                                                , describe
                                                )
import           Test.QuickCheck                ( Arbitrary(arbitrary)
                                                , elements
                                                )
import           Types                          ( Alert(Alert)
                                                , TemperatureOverview(..)
                                                , WeatherResponse
                                                    ( WeatherResponse
                                                    )
                                                )

instance Arbitrary Text where
    arbitrary = pack <$> arbitrary
instance Arbitrary Alert where
    arbitrary = Alert <$> arbitrary <*> arbitrary
instance Arbitrary TemperatureOverview where
    arbitrary = elements [Cold, Moderate, Hot]
instance Arbitrary WeatherResponse where
    arbitrary = WeatherResponse <$> arbitrary <*> arbitrary <*> arbitrary

spec :: Spec
spec = describe "adheres to api contract" $ validateEveryToJSON weatherApi
