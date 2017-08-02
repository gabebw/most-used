module MostUsed.MostUsedSpec
    ( main
    , spec
    ) where

import Data.List (intercalate)
import MostUsed
import MostUsed.Parser.Zsh as Zsh
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
    describe "failures" $ do
        it "returns a pretty error message" $ do
            let s = "bad"
            failures Zsh.items s `shouldBe` ["bad:1:1:\nunexpected 'b'\nexpecting white space\n"]

        it "does not return any successfully parsed commands" $ do
            let s = cmd "cmd 'a'"
            failures Zsh.items s `shouldBe` []

    describe "successes" $ do
        it "ignores a single unparseable command" $ do
            successes Zsh.items unparseableCommand `shouldBe` []

        it "ignores an unparseable command but parses the good one" $ do
            let s1 = unparseableCommand
            let s2 = cmd "cmd 'a'"
            let s = intercalate "\n" [s1, s2]
            successes Zsh.items s `shouldBe` [Item "cmd" [SingleQuoted "a"]]

        describe "with a single item" $ do
            it "parses a command with no arguments" $ do
                let s = cmd "cmd"
                let result = Item "cmd" []
                successes Zsh.items s `shouldBe` [result]

            it "parses a command with no arguments and a low number" $ do
                let s = cmd2 "cmd"
                let result = Item "cmd" []
                successes Zsh.items s `shouldBe` [result]

            it "parses a command with one unquoted argument" $ do
                let s = cmd "cmd arg"
                let result = Item "cmd" [NotQuoted "arg"]
                successes Zsh.items s `shouldBe` [result]

            it "parses a command with one single-quoted argument" $ do
                let s = cmd "cmd 'a r g'"
                let result = Item "cmd" [SingleQuoted "a r g"]
                successes Zsh.items s `shouldBe` [result]

            it "parses a command with one double-quoted argument" $ do
                let s = cmd "cmd \"a r g\""
                let result = Item "cmd" [DoubleQuoted "a r g"]
                successes Zsh.items s `shouldBe` [result]

            it "parses a command with one backtick-ed argument" $ do
                let s = cmd "cmd `arg`"
                let result = Item "cmd" [Backticks "arg"]
                successes Zsh.items s `shouldBe` [result]

            it "parses a command with one $(argument)" $ do
                let s = cmd "cmd $(arg x)"
                let result = Item "cmd" [CommandSubstitution "arg x"]
                successes Zsh.items s `shouldBe` [result]

            it "parses a command preceded by a backslash" $ do
                let s = cmd "\\psql arg"
                let result = Item "\\psql" [NotQuoted "arg"]
                successes Zsh.items s `shouldBe` [result]

            it "parses a command with process substitution" $ do
                let s = cmd "cmd <(one) <(two)"
                let result = Item "cmd" [ProcessSubstitution "one"
                                        , ProcessSubstitution "two"]
                successes Zsh.items s `shouldBe` [result]

            it "parses a command with a variety of arguments" $ do
                let s = cmd "cmd one `two` '3 x' \"4 y\" $(arg)"
                let result = Item "cmd" [NotQuoted "one"
                                        , Backticks "two"
                                        , SingleQuoted "3 x"
                                        , DoubleQuoted "4 y"
                                        , CommandSubstitution "arg"]
                successes Zsh.items s `shouldBe` [result]

            it "parses a line with multiple items separated by a pipe" $ do
                let s = cmd "c1 one |c2 `two`"
                let result = [Item "c1" [NotQuoted "one"]
                             , Item "c2" [Backticks "two"]]
                successes Zsh.items s `shouldBe` result

            it "parses a line with @" $ do
                let s = cmd "echo @gabebw"
                let result = [Item "echo" [NotQuoted "@gabebw"]]

                successes Zsh.items s `shouldBe` result

            it "parses a line with $-quoting" $ do
                let s = cmd "echo $' hello '"
                let result = [Item "echo" [SingleQuoted " hello "]]

                successes Zsh.items s `shouldBe` result

            it "parses a line with a variable" $ do
                let s = cmd "echo $var"
                let result = [Item "echo" [NotQuoted "$var"]]

                successes Zsh.items s `shouldBe` result

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
                successes Zsh.items s `shouldBe` [i1, i2, Item "c3" []]

        it "can parse something with a bare escaped newline" $ do
            let s = cmd "fc -lDt 1 \\n"
            let item = Item "fc" [NotQuoted "-lDt", NotQuoted "1"]

            successes Zsh.items s `shouldBe` [item]

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
                successes Zsh.items s `shouldBe` [i1, i2, Item "c3" []]
