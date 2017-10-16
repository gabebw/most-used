module MostUsed.MostUsedSpec
    ( main
    , spec
    ) where

import           Data.List              (intercalate)
import           MostUsed
import qualified MostUsed.Parser.Bash   as Bash
import qualified MostUsed.Parser.Zsh    as Zsh
import           MostUsed.Types
import           Test.Hspec
import           Text.Megaparsec.String

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "top level" $ do
    commonSpec "Bash parser" Bash.items bashCommand
    commonSpec "Zsh parser" Zsh.items zshCommand
    zshSpec Zsh.items zshCommand

commonSpec :: String -> Parser [Command] -> (String -> String) -> Spec
commonSpec description parser cmd = describe description $ do
    describe "failures" $ do
        it "returns a pretty error message" $ do
            let s = "||||"
            head (failures parser s) `shouldStartWith` "||||:1:1"

        it "does not return any successfully parsed commands" $ do
            let s = cmd "cmd 'a'"
            failures parser s `shouldBe` []

    describe "successes" $ do
        it "ignores a single unparseable command" $ do
            successes parser (unparseableCommand cmd) `shouldBe` []

        it "ignores an unparseable command but parses the good one" $ do
            let s1 = unparseableCommand cmd
            let s2 = cmd "cmd 'a'"
            let s = intercalate "\n" [s1, s2]
            successes parser s `shouldBe` [Command "cmd" [SingleQuoted "a"]]

        describe "with a single item" $ do
            it "parses a command with no arguments" $ do
                let s = cmd "cmd"
                let result = Command "cmd" []
                successes parser s `shouldBe` [result]

            it "parses a command with one unquoted argument" $ do
                let s = cmd "cmd arg"
                let result = Command "cmd" [NotQuoted "arg"]
                successes parser s `shouldBe` [result]

            it "parses a command with one single-quoted argument" $ do
                let s = cmd "cmd 'a r g'"
                let result = Command "cmd" [SingleQuoted "a r g"]
                successes parser s `shouldBe` [result]

            it "parses a command with one double-quoted argument" $ do
                let s = cmd "cmd \"a r g\""
                let result = Command "cmd" [DoubleQuoted "a r g"]
                successes parser s `shouldBe` [result]

            it "parses a command with one backtick-ed argument" $ do
                let s = cmd "cmd `arg`"
                let result = Command "cmd" [Backticks "arg"]
                successes parser s `shouldBe` [result]

            it "parses a command with one $(argument)" $ do
                let s = cmd "cmd $(arg x)"
                let innerCommand = Command "arg" [NotQuoted "x"]
                let result = Command "cmd" [CommandSubstitution innerCommand]
                successes parser s `shouldBe` [result]

            it "parses a command with nested $(argument $(arg))" $ do
                let s = cmd "cmd $(arg1 $(arg2))"
                let inner = Command "arg2" []
                let cmd1 = Command "arg1" [CommandSubstitution inner]
                let result = Command "cmd" [CommandSubstitution cmd1]

                successes parser s `shouldBe` [result]

            it "parses a command preceded by a backslash" $ do
                let s = cmd "\\psql arg"
                let result = Command "\\psql" [NotQuoted "arg"]
                successes parser s `shouldBe` [result]

            it "parses a command with process substitution" $ do
                let s = cmd "cmd <(one 'a') <(two)"
                let one = Command "one" [SingleQuoted "a"]
                let two = Command "two" []
                let result = Command "cmd" (map ProcessSubstitution [one, two])
                successes parser s `shouldBe` [result]

            it "parses a command with a variety of arguments" $ do
                let s = cmd "cmd one `two` '3 x' \"4 y\" $(arg)"
                let result = Command "cmd" [NotQuoted "one"
                                        , Backticks "two"
                                        , SingleQuoted "3 x"
                                        , DoubleQuoted "4 y"
                                        , CommandSubstitution (Command "arg" [])]
                successes parser s `shouldBe` [result]

            it "parses a line with multiple items separated by a pipe" $ do
                let s = cmd "c1 one |c2 `two`"
                let result = [Command "c1" [NotQuoted "one"]
                             , Command "c2" [Backticks "two"]]
                successes parser s `shouldBe` result

            it "parses a line with @" $ do
                let s = cmd "echo @gabebw"
                let result = [Command "echo" [NotQuoted "@gabebw"]]

                successes parser s `shouldBe` result

            it "parses a line with $-quoting" $ do
                let s = cmd "echo $' hello '"
                let result = [Command "echo" [SingleQuoted " hello "]]

                successes parser s `shouldBe` result

            it "parses a line with a variable" $ do
                let s = cmd "echo $var"
                let result = [Command "echo" [NotQuoted "$var"]]

                successes parser s `shouldBe` result

            it "parses a command with a heredoc" $ do
                let s = cmd "cmd <<<hello"
                let result = Command "cmd" [Heredoc "hello"]

                successes parser s `shouldBe` [result]

            it "parses a command with a multi-line single-quoted heredoc" $ do
                let s = cmd "cmd <<<'hello\\nthere\\n'"
                let result = Command "cmd" [Heredoc "hello\\nthere\\n"]

                successes parser s `shouldBe` [result]

            it "parses a command with a multi-line double-quoted heredoc" $ do
                let s = cmd "cmd <<<\"hello\\nthere\\n\""
                let result = Command "cmd" [Heredoc "hello\\nthere\\n"]

                successes parser s `shouldBe` [result]

        describe "with multiple items" $ do
            describe "each of which is on its own line" $ do
                it "parses commands with a variety of arguments" $ do
                    let s1 = cmd "c1 one `two` $(arg)"
                    let s2 = cmd "c2 '3 x' \"4 y\""
                    let s3 = cmd "c3"
                    let s = intercalate "\n" [s1, s2, s3]
                    let i1 = Command "c1" [NotQuoted "one"
                                    , Backticks "two"
                                    , CommandSubstitution (Command "arg" [])]
                    let i2 = Command "c2" [SingleQuoted "3 x"
                                    , DoubleQuoted "4 y"]
                    successes parser s `shouldBe` [i1, i2, Command "c3" []]

            describe "separated by a semicolon" $ do
                it "parses commands with a variety of arguments" $ do
                    let s1 = "c1 one `two` $(arg)"
                    let s2 = "c2 '3 x' \"4 y\""
                    let s3 = "c3"
                    -- Note that there's only one `cmd` prefix, since Zsh only
                    -- adds the prefix for items on a new line, and these items
                    -- are all on the same line.
                    let s = cmd $ intercalate ";" [s1, s2, s3]
                    let i1 = Command "c1" [NotQuoted "one"
                                    , Backticks "two"
                                    , CommandSubstitution (Command "arg" [])]
                    let i2 = Command "c2" [SingleQuoted "3 x"
                                    , DoubleQuoted "4 y"]
                    successes parser s `shouldBe` [i1, i2, Command "c3" []]

        it "can parse something with a bare escaped newline" $ do
            let s = cmd "fc -lDt 1 \\n"
            let item = Command "fc" [NotQuoted "-lDt", NotQuoted "1"]

            successes parser s `shouldBe` [item]

zshSpec :: Parser [Command] -> (String -> String) -> Spec
zshSpec parser cmd = describe "Zsh-only tests" $ do
    describe "failures" $ do
        it "does not parse a string without a command number" $ do
            let s = "no-number"
            head (failures parser s) `shouldStartWith` "no-number:1:1"

    describe "successes" $ do
        describe "with a single item" $ do
            it "parses a command with no arguments and a low number" $ do
                let s = zshCommandWithLowNumber "cmd"
                let result = Command "cmd" []
                successes parser s `shouldBe` [result]

        describe "with multiple items, each of which is on one line" $ do
            it "parses commands with a variety of arguments" $ do
                let s1 = cmd "c1 one `tw\\no` $(arg)"
                let s2 = cmd "c2 '3 x' \"4 y\""
                let s3 = cmd "c3"
                let s = intercalate "\n" [s1, s2, s3]
                let i1 = Command "c1" [NotQuoted "one"
                                   , Backticks "tw\\no"
                                   , CommandSubstitution (Command "arg" [])]
                let i2 = Command "c2" [SingleQuoted "3 x"
                                   , DoubleQuoted "4 y"]
                successes parser s `shouldBe` [i1, i2, Command "c3" []]

        it "can parse something with a bare escaped newline" $ do
            let s = cmd "fc -lDt 1 \\n"
            let item = Command "fc" [NotQuoted "-lDt", NotQuoted "1"]

            successes parser s `shouldBe` [item]

        describe "with multiple items, not all of which are on one line" $ do
            it "parses commands with a variety of arguments" $ do
                let s1 = cmd "c1 one " ++ escapedNewline ++ "`tw" ++ escapedNewline ++ "no` $(arg)"
                let s2 = cmd "c2 '3" ++ escapedNewline ++ " x' \"4 " ++ escapedNewline ++ "y\""
                let s3 = cmd "c3"
                let s = intercalate "\n" [s1, s2, s3]
                let i1 = Command "c1" [NotQuoted "one"
                                   , Backticks "tw\\nno"
                                   , CommandSubstitution (Command "arg" [])]
                let i2 = Command "c2" [SingleQuoted "3\\n x"
                                   , DoubleQuoted "4 \\ny"]
                successes parser s `shouldBe` [i1, i2, Command "c3" []]

escapedNewline :: String
escapedNewline = "\\n"

unparseableCommand :: (String -> String) -> String
unparseableCommand cmd = cmd "foo(){\\n # -X = \"whatever\", blah\\n"

bashCommand :: String -> String
bashCommand = id

zshCommand :: String -> String
zshCommand s = commandNumber ++ s
    where
        commandNumber = "  452  "

zshCommandWithLowNumber :: String -> String
zshCommandWithLowNumber s = "    1  " ++ s
