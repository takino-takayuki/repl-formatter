module Formatter.ParserSpec (spec) where

import Test.Hspec
import Formatter.Parser (parseOutput) -- 実装対象のモジュールをインポート

spec :: Spec
spec = do
  describe "parseOutput" $ do
    it "should correctly parse a simple integer output" $ do
      let input = "λ> 100\n100\nλ>"
      -- 現状は、Parser.hsの実装がエラーを返すため、ここではテストをスキップします。
      -- 実装が進んだら、このテストを有効化します。
      pendingWith "Test implementation is pending until parser logic is written."
      
    it "should parse a list output" $ do
      let input = "λ> [1, 2, 3]\n[1, 2, 3]\nλ>"
      -- pendingWith "Test implementation is pending."
      pendingWith "Test implementation is pending until parser logic is written."

    -- TODO: 今後、レコード構文、データ型、エラーメッセージなどのパーステストケースを追加します。
    pure ()
