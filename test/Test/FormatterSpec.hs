module Test.FormatterSpec (formatterSpec) where

import Test.Hspec
import Data.Text (Text, pack)

-- テスト対象のモジュールをインポート
import Formatter.Core (formatForRepl, FormatMode(..))

-- 'Data.Text' の値を比較しやすくするためのヘルパー関数
shouldFormatAs :: Text -> Text -> Expectation
shouldFormatAs input expected = formatForRepl input NormalMode `shouldBe` expected

-- REPL向け整形ロジックのテストスイート
formatterSpec :: Spec
formatterSpec = do
    describe "Single Line Mode (NormalMode: Default Behavior)" $ do

        -- 1. シンプルな式 (そのまま返す)
        it "should return a simple expression as is" $
            "putStrLn \"Hello\"" `shouldFormatAs` "putStrLn \"Hello\""

        -- 2. コメントのみの行 (空行を返す)
        it "should return empty string for comment-only line" $
            "-- This is a comment" `shouldFormatAs` ""

        -- 3. '>>>' を含むHaddockコメントの解析 (コード部分を返す)
        it "should extract code after '-- >>>' (haddock test)" $
            "-- >>> myFunc 5" `shouldFormatAs` "myFunc 5"

        -- 4. インラインコメントの削除 (コード部分を返す)
        it "should remove trailing single-line comment" $
            "x = 5 + 10 -- Calculation" `shouldFormatAs` "x = 5 + 10"
        
        -- 5. 複数のコメントがある行 (最初のコード部分のみを返す)
        it "should only remove the first comment marker in a complex line" $
            "let x = 1 -- comment1 -- comment2" `shouldFormatAs` "let x = 1"

    describe "Multi Line Mode (NormalMode: Default Behavior)" $ do
        
        let multiLineCode = pack $ unlines [
                "myLongFunc arg1 =",
                "  let temp = arg1 * 2",
                "  in temp + 1"
                ]
        
        let expectedMultiLine = pack $ unlines [
                ":{",
                "myLongFunc arg1 =",
                "  let temp = arg1 * 2",
                "  in temp + 1",
                ":}"
                ]

        -- 6. 複数行のコードのブロックエスケープ
        it "should wrap multi-line code with :{\\n ... \\n:}" $
            multiLineCode `shouldFormatAs` expectedMultiLine

        -- 7. 複数行だが、コメント行を含む場合の整形（改行数で判定、全体をエスケープ）
        let commentedMultiLine = pack $ unlines [
                "-- My function definition",
                "data MyType = MyValue",
                "  deriving Show"
                ]

        let expectedCommentedMultiLine = pack $ unlines [
                ":{",
                "-- My function definition",
                "data MyType = MyValue",
                "  deriving Show",
                ":}"
                ]

        it "should wrap multi-line code even if it contains comments" $
            commentedMultiLine `shouldFormatAs` expectedCommentedMultiLine
