module Main (main, spec) where

import Test.Hspec
import MostUsed

main :: IO ()
main = hspec spec

timestamp :: String
timestamp = ": 1401927488:0;"

spec :: Spec
spec = do
    describe "parseHistory" $ do
        describe "with a single item" $ do
            it "parses a command with no arguments" $ do
                let s = timestamp ++ "cmd"
                let result = Item "cmd" []
                parseHistory s `shouldBe` [result]

            it "parses a command with one unquoted argument" $ do
                let s = timestamp ++ "cmd arg"
                let result = Item "cmd" [NotQuoted "arg"]
                parseHistory s `shouldBe` [result]

            it "parses a command with one single-quoted argument" $ do
                let s = timestamp ++ "cmd 'a r g'"
                let result = Item "cmd" [SingleQuoted "a r g"]
                parseHistory s `shouldBe` [result]

            it "parses a command with one double-quoted argument" $ do
                let s = timestamp ++ "cmd \"a r g\""
                let result = Item "cmd" [DoubleQuoted "a r g"]
                parseHistory s `shouldBe` [result]

            it "parses a command with one backtick-ed argument" $ do
                let s = timestamp ++ "cmd `arg`"
                let result = Item "cmd" [Backticks "arg"]
                parseHistory s `shouldBe` [result]

            it "parses a command with one $(argument)" $ do
                let s = timestamp ++ "cmd $(arg x)"
                let result = Item "cmd" [CommandSubstitution "arg x"]
                parseHistory s `shouldBe` [result]

            it "parses a command with a variety of arguments" $ do
                let s = timestamp ++ "cmd one `two` '3 x' \"4 y\" $(arg)"
                let result = Item "cmd" [NotQuoted "one"
                                        , Backticks "two"
                                        , SingleQuoted "3 x"
                                        , DoubleQuoted "4 y"
                                        , CommandSubstitution "arg"]
                parseHistory s `shouldBe` [result]

        describe "with multiple items, each of which is on one line" $ do
            it "parses commands with a variety of arguments" $ do
                let s1 = timestamp ++ "c1 one `two` $(arg)"
                let s2 = timestamp ++ "c2 '3 x' \"4 y\""
                let s3 = timestamp ++ "c3"
                let s = s1 ++ "\n" ++ s2 ++ "\n" ++ s3
                let i1 = Item "c1" [NotQuoted "one"
                                   , Backticks "two"
                                   , CommandSubstitution "arg"]
                let i2 = Item "c2" [SingleQuoted "3 x"
                                   , DoubleQuoted "4 y"]
                parseHistory s `shouldBe` [i1, i2, Item "c3" []]

        describe "with multiple items, not all of which are on one line" $ do
            it "parses commands with a variety of arguments" $ do
                let s1 = timestamp ++ "c1 one " ++ "\n" ++ "`tw\no` $(arg)"
                let s2 = timestamp ++ "c2 '3" ++ "\n" ++ " x' \"4 " ++ "\n" ++ "y\""
                let s3 = timestamp ++ "c3"
                let s = s1 ++ "\n" ++ s2 ++ "\n" ++ s3
                let i1 = Item "c1" [NotQuoted "one"
                                   , Backticks "tw\no"
                                   , CommandSubstitution "arg"]
                let i2 = Item "c2" [SingleQuoted "3\n x"
                                   , DoubleQuoted "4 \ny"]
                parseHistory s `shouldBe` [i1, i2, Item "c3" []]
