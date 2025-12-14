# test-key-generator.sh (修正案)

# 1. 実行可能ファイルのフルパスを取得
#    cabal exec which の結果を EXECUTABLE_PATH に代入
EXECUTABLE_PATH=$(cabal exec which key-generator)

# 2. パスが取得できているかチェック（デバッグ用）
if [ -z "$EXECUTABLE_PATH" ]; then
    echo "DEBUG ERROR: key-generator の実行パスが取得できませんでした。"
    exit 1
fi

# 3. Haskellプログラムを実行し、その出力を source コマンドで取り込む
#    <( ... ) はプロセスの出力を一時ファイルのように扱うプロセス置換です。
. <( $EXECUTABLE_PATH )
# または、シンプルにパイプを使用することもできます
# $EXECUTABLE_PATH | source /dev/stdin 
# ただし、<( ... ) の方が現代的なBashで推奨されます。

echo "--- Haskellから取り込んだJSONキー ---"
echo "INPUT_VALUEキー: $JSON_KEY_INPUT_VALUE"
echo "OUTPUT_RESULTキー: $JSON_KEY_OUTPUT_RESULT"
echo "OUTPUT_STATUSキー: $JSON_KEY_OUTPUT_STATUS"

# 実行例: 取り込んだキーを使ってJSONを作成
# jqのキーアクセスも修正が必要です（キーが空のため : を使用していましたが、ここでは $JSON_KEY_INPUT_VALUE を使います）
INPUT_JSON=$(jq -n \
              --arg key "$JSON_KEY_INPUT_VALUE" \
              --argjson val 100 \
              '. | .[$key] = $val')

echo -e "\n--- 生成されたJSON ---"
echo "$INPUT_JSON"
