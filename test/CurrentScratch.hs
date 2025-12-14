module CurrentScratch where

-- --- スクラッチ履歴管理 (scratch-ctl save時に自動タグ付け) ---
-- 以下の行のコメントを解除し、タグ名とステータスを記述してください。
-- 推奨ステータス: stable, provisional, draft, deprecated
-- 例: -- @tag: MyParserFeature@stable
-- 例: -- @tag: QuickTest (ステータス未指定の場合、自動で '@draft' が付与されます)

-- @tag: base base2@provisional

import Test.Hspec

parseOutput :: String -> String
parseOutput = error "Not Implemented Yet: Formatter.Parser.parseOutput"

spec :: Spec
spec = do
  describe "parseOutput" $ do
    it "current scratch" $ do
      let input = "λ> 100\n100\nλ>"
      print input
      -- 現状は、Parser.hsの実装がエラーを返すため、ここではテストをスキップします。
      -- 実装が進んだら、このテストを有効化します。
      pendingWith "current scratch out !!!"
      

main :: IO ()
main = hspec spec

three :: Semigroup a => a -> a
three n = n <> n <> n

{-
main 
hspec spec
three [3]
 -}
