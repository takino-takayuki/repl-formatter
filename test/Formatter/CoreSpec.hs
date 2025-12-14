module Formatter.CoreSpec (spec) where

import Test.Hspec
import Formatter.Core (formatOutput) -- 実装対象のモジュールをインポート

spec :: Spec
spec = do
  describe "formatOutput" $ do
    it "should return a simple success message for simple input" $ do
      let simpleReplOutput = "λ> 1 + 1\n2\nλ>"
      -- 現状は、Core.hsの実装がエラーを返すため、ここではテストをスキップします。
      -- 実装が進んだら、このテストを有効化します。
      pendingWith "Test implementation is pending until core logic is written."
      
    it "should correctly format a multi-line output" $ do
      let multiLineOutput = "λ> putStrLn \"Hello\" >> putStrLn \"World\"\nHello\nWorld\nλ>"
      -- pendingWith "Test implementation is pending."
      pendingWith "Test implementation is pending until core logic is written."

    -- TODO: 今後、エラーメッセージの整形、リストの整形などのテストケースを追加します。
    pure ()
