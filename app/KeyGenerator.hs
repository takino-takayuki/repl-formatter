{-# LANGUAGE OverloadedStrings #-} -- ★ 1. GHC拡張機能

-- ファイル名: app/KeyGenerator.hs -- ★ 2. コメント行を拡張宣言の直下に移動

module Main where -- ★ 3. モジュール宣言

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Exit (exitSuccess)


-- =======================================================
-- JSONキー名定義 (マスターソース)
-- =======================================================

-- INPUT KEYS (processor.exeが受け取る入力JSONのキー)
inputKey_Value :: Text
inputKey_Value = "input_value"

-- OUTPUT KEYS (processor.exeが出力するJSONのキー)
outputKey_Status :: Text
outputKey_Status = "status"
outputKey_Result :: Text
outputKey_Result = "result"
outputKey_Message :: Text
outputKey_Message = "message"

-- =======================================================
-- メイン関数
-- =======================================================

-- Bashが読み込める形式（export VAR="value"）でキー名を出力する
main :: IO ()
main = do
    -- 入力キーの出力
    outputExport "JSON_KEY_INPUT_VALUE" inputKey_Value

    -- 出力キーの出力
    outputExport "JSON_KEY_OUTPUT_STATUS" outputKey_Status
    outputExport "JSON_KEY_OUTPUT_RESULT" outputKey_Result
    outputExport "JSON_KEY_OUTPUT_MESSAGE" outputKey_Message
    
    exitSuccess
    
-- exportコマンド文字列を生成し、標準出力に出力するヘルパー関数
outputExport :: String -> Text -> IO ()
outputExport bashVarName textValue =
    TIO.putStrLn $ T.concat 
        [ T.pack "export "
        , T.pack bashVarName 
        , T.pack "=\""
        , textValue 
        , T.pack "\""
        ]
