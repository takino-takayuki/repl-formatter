#!/bin/bash

PROCESSOR="$EXECUTABLE_PATH" # 実行可能ファイル名に置き換え (例: ./processor)

echo "--- 実行ケース 1: 成功 (入力値 42) ---"
SUCCESS_JSON='{"inputValue": 42}'

# Haskellプログラムを実行
OUTPUT=$(echo "$SUCCESS_JSON" | $PROCESSOR)
EXIT_CODE=$?

echo "出　力: $OUTPUT"
echo "終了コード: $EXIT_CODE"

if [ $EXIT_CODE -eq 0 ]; then
    echo "検証結果: 成功。結果の値: $(echo $OUTPUT | jq -r '.result')"
else
    echo "検証結果: 失敗。予期せぬエラーです。"
fi

echo "----------------------------------------"
