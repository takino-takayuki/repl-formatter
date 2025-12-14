module Main (main) where

import Test.Hspec
-- 衝突を避けるため、修飾インポート (qualified) を使用
import qualified Formatter.CoreSpec
import qualified Formatter.ParserSpec
import qualified CurrentScratch
-- ... 他のテストモジュールもここに追加します

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  -- 修飾子を使って、どのモジュールの 'spec' 関数を使うかを明確に
  -- describe "Formatter.Core" Formatter.CoreSpec.spec
  -- describe "Formatter.Parser" Formatter.ParserSpec.spec
  describe "scratch" CurrentScratch.spec
  
  -- 必要に応じて、今後ここにテストスイートを追加
  pure ()
