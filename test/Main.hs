module Main (main, spec) where

import Data.List (intercalate)
import MostUsed
import Test.Hspec

commandNumber :: String
commandNumber = "  452  "

unparseableCommand :: String
unparseableCommand = cmd "foo(){\\n # -X = \"whatever\", blah\\n"

cmd :: String -> String
cmd s = commandNumber ++ s

cmd2 :: String -> String
cmd2 s = "    1  " ++ s

main :: IO ()
main = hspec spec

escapedNewline :: String
escapedNewline = "\\n"

spec :: Spec
spec = do
    describe "parseHistory'" $ do
        it "ignores a single unparseable command" $ do
            parseHistory' unparseableCommand `shouldBe` []

        it "ignores an unparseable command but parses the good one" $ do
            let s1 = unparseableCommand
            let s2 = cmd "cmd 'a'"
            let s = intercalate "\n" [s1, s2]
            parseHistory' s `shouldBe` [Item "cmd" [SingleQuoted "a"]]

    describe "parseHistory" $ do
        describe "with a single item" $ do
            it "parses a command with no arguments" $ do
                let s = cmd "cmd"
                let result = Item "cmd" []
                parseHistory s `shouldBe` Right [result]

            it "parses a command with no arguments and a low number" $ do
                let s = cmd2 "cmd"
                let result = Item "cmd" []
                parseHistory s `shouldBe` Right [result]

            it "parses a command with one unquoted argument" $ do
                let s = cmd "cmd arg"
                let result = Item "cmd" [NotQuoted "arg"]
                parseHistory s `shouldBe` Right [result]

            it "parses a command with one single-quoted argument" $ do
                let s = cmd "cmd 'a r g'"
                let result = Item "cmd" [SingleQuoted "a r g"]
                parseHistory s `shouldBe` Right [result]

            it "parses a command with one double-quoted argument" $ do
                let s = cmd "cmd \"a r g\""
                let result = Item "cmd" [DoubleQuoted "a r g"]
                parseHistory s `shouldBe` Right [result]

            it "parses a command with one backtick-ed argument" $ do
                let s = cmd "cmd `arg`"
                let result = Item "cmd" [Backticks "arg"]
                parseHistory s `shouldBe` Right [result]

            it "parses a command with one $(argument)" $ do
                let s = cmd "cmd $(arg x)"
                let result = Item "cmd" [CommandSubstitution "arg x"]
                parseHistory s `shouldBe` Right [result]

            it "parses a command with process substitution" $ do
                let s = cmd "cmd <(one) <(two)"
                let result = Item "cmd" [ProcessSubstitution "one"
                                        , ProcessSubstitution "two"]
                parseHistory s `shouldBe` Right [result]

            it "parses a command with a variety of arguments" $ do
                let s = cmd "cmd one `two` '3 x' \"4 y\" $(arg)"
                let result = Item "cmd" [NotQuoted "one"
                                        , Backticks "two"
                                        , SingleQuoted "3 x"
                                        , DoubleQuoted "4 y"
                                        , CommandSubstitution "arg"]
                parseHistory s `shouldBe` Right [result]

        describe "with multiple items, each of which is on one line" $ do
            it "parses commands with a variety of arguments" $ do
                let s1 = cmd "c1 one `tw\\no` $(arg)"
                let s2 = cmd "c2 '3 x' \"4 y\""
                let s3 = cmd "c3"
                let s = intercalate "\n" [s1, s2, s3]
                let i1 = Item "c1" [NotQuoted "one"
                                   , Backticks "tw\\no"
                                   , CommandSubstitution "arg"]
                let i2 = Item "c2" [SingleQuoted "3 x"
                                   , DoubleQuoted "4 y"]
                parseHistory s `shouldBe` Right [i1, i2, Item "c3" []]

        describe "with multiple items, not all of which are on one line" $ do
            it "parses commands with a variety of arguments" $ do
                let s1 = cmd "c1 one " ++ escapedNewline ++ "`tw" ++ escapedNewline ++ "no` $(arg)"
                let s2 = cmd "c2 '3" ++ escapedNewline ++ " x' \"4 " ++ escapedNewline ++ "y\""
                let s3 = cmd "c3"
                let s = intercalate "\n" [s1, s2, s3]
                let i1 = Item "c1" [NotQuoted "one"
                                   , Backticks "tw\\nno"
                                   , CommandSubstitution "arg"]
                let i2 = Item "c2" [SingleQuoted "3\\n x"
                                   , DoubleQuoted "4 \\ny"]
                parseHistory s `shouldBe` Right [i1, i2, Item "c3" []]
