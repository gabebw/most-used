module MostUsed.CLI.DisplaySpec
    ( main
    , spec
    ) where

import           MostUsed.CLI.Display (failedParses, successfulParses)
import qualified MostUsed.Parser.Bash as B
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "MostUsed.CLI.Display" $ do
    describe "successfulParses" $ do
        describe "when none of the commands include first argument" $ do
            it "returns a string describing the commands" $ do
                let ss = ["bar", "foo", "foo", "baz", "bar", "foo"]
                let expected = "2 hello\n1 hi"
                let result = successfulParses [] B.items "hello\nhi <<<hello\nhello"
                result `shouldBe` expected

        describe "when some commands include first argument" $ do
            it "fails so far" $ do
                3 `shouldBe` 2
