{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as B
import System.Exit (exitWith, ExitCode(ExitFailure, ExitSuccess))
import System.IO (hPutStrLn, stderr) -- デバッグ目的
import Control.Monad (unless)
import Data.Text (Text)
import qualified Data.Text as T

-- ----------------------------------------------------
-- 1. データ型定義 (入出力)
-- ----------------------------------------------------

-- 入力JSONの型: {"input_value": 42}
data InputData = InputData
    { inputValue :: Int
    } deriving (Show, Generic, FromJSON)

-- 出力JSONの型: 成功時と失敗時で共通の構造を使う
data OutputResult = OutputResult
    { status :: Text
    , result :: Maybe Int   -- 成功時のみ値を持つ (失敗時はNothing)
    , message :: Text       -- 成功時/失敗時のメッセージ
    } deriving (Show, Generic, ToJSON)

-- ----------------------------------------------------
-- 2. メインロジック
-- ----------------------------------------------------

-- 成功時の結果を作成
successResult :: Int -> OutputResult
successResult res = OutputResult
    { status = "success"
    , result = Just res
    , message = "Value successfully incremented."
    }

-- 失敗時の結果を作成し、デバッグコードを出力して終了
failureExit :: String -> IO ()
failureExit debugMsg = do
    -- デバッグコードを含むエラーJSONを作成
    let errorResult = OutputResult
            { status = "error"
            , result = Nothing
            , message = T.pack $ "DEBUG ERROR: " ++ debugMsg
            }
    -- 標準出力にエラーJSONを出力
    B.putStrLn $ encode errorResult
    
    -- 標準エラー出力に終了情報を出力 (より詳細なデバッグ情報用)
    hPutStrLn stderr $ "Process aborted with: " ++ debugMsg

    -- 非ゼロの終了コードで終了
    exitWith (ExitFailure 1)

-- JSONを受け取り、処理を実行する関数
runProcessor :: InputData -> IO OutputResult
runProcessor input = do
    let val = inputValue input
    
    -- 厳格なチェック例: 入力値が0以下でないこと
    unless (val > 0) $
        failureExit $ "Input value " ++ show val ++ " is not positive."

    -- 処理: 値を1増やす
    let resultValue = val + 1

    return $ successResult resultValue

main :: IO ()
main = do
    -- 標準入力からLazy ByteStringとしてJSONを読み込む
    inputJson <- B.getContents
    
    -- JSONデコードの厳格なチェック
    case eitherDecode inputJson of
        Left err -> 
            -- デコード失敗（JSON形式がおかしい、または必須フィールドがない）
            failureExit $ "JSON decoding failed: " ++ err
            
        Right inputData -> do
            -- 内部ロジックを実行
            output <- runProcessor inputData
            
            -- 成功JSONを標準出力に出力
            B.putStrLn $ encode output
            
            -- 正常終了コード 0 で終了
            exitWith ExitSuccess
