module MostUsed.CLI.UtilSpec
    ( main
    , spec
    ) where

import qualified Data.HashMap.Strict as HM
import           MostUsed.CLI.Util   (buildMap)
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "MostUsed.CLI.Util" $ do
    describe "buildMap" $ do
        it "builds a HashMap of the counts of each string" $ do
            let ss = ["bar", "foo", "foo", "baz", "bar", "foo"]
            let expected = HM.fromList [("foo", 3), ("bar", 2), ("baz", 1)]
            buildMap ss `shouldBe` expected
