module ConfigSpec where

import           Config                         ( loadConfig )
import           Data.Functor                   ( void )
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , shouldReturn
                                                )

spec :: Spec
spec = do
    it "load the app config" $ do
        void loadConfig `shouldReturn` ()
