module ReplUtils where

import System.Process (readProcess)
import System.IO (hPutStrLn, stderr)

-- シェルコマンド実行関数（以前の sh の定義）
sh :: String -> IO String
sh command = readProcess "sh" ["-c", command] ""

-- その他のカスタム関数
test_out :: String -> IO ()
test_out s = hPutStrLn stderr ("REPL UTIL: " ++ s)
