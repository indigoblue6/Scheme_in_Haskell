#!/bin/bash
# SICP 全例題テストスクリプト

echo "========================================="
echo "SICP Comprehensive Test Suite"
echo "========================================="
echo ""

INTERPRETER="cabal run -v0 scheme-interpreter"
PASS=0
FAIL=0
TOTAL=0

# テスト実行関数
run_test() {
    local test_name=$1
    local test_file=$2
    
    echo "Testing: $test_name"
    TOTAL=$((TOTAL + 1))
    
    if timeout 30 $INTERPRETER < "$test_file" > /tmp/sicp_test_output.txt 2>&1; then
        if grep -q "Parse error\|Error:" /tmp/sicp_test_output.txt; then
            echo "  ❌ FAIL - Errors detected"
            FAIL=$((FAIL + 1))
            grep -E "Parse error|Error:" /tmp/sicp_test_output.txt | head -3
        else
            echo "  ✅ PASS"
            PASS=$((PASS + 1))
        fi
    else
        echo "  ❌ FAIL - Timeout or crash"
        FAIL=$((FAIL + 1))
    fi
    echo ""
}

# 既存のテストファイルを実行
if [ -f "sicp_examples.scm" ]; then
    run_test "SICP Basic Examples" "sicp_examples.scm"
fi

if [ -f "sicp_additional.scm" ]; then
    run_test "SICP Additional Examples" "sicp_additional.scm"
fi

if [ -f "sicp_chapter2.scm" ]; then
    run_test "SICP Chapter 2 - Data Abstraction" "sicp_chapter2.scm"
fi

if [ -f "sicp_chapter3.scm" ]; then
    run_test "SICP Chapter 3 - Modularity & State" "sicp_chapter3.scm"
fi

if [ -f "sicp_chapter4.scm" ]; then
    run_test "SICP Chapter 4 - Metalinguistic Abstraction" "sicp_chapter4.scm"
fi

if [ -f "sicp_chapter5.scm" ]; then
    run_test "SICP Chapter 5 - Register Machines" "sicp_chapter5.scm"
fi

# テストディレクトリのテスト
for test_file in tests/*.scm; do
    if [ -f "$test_file" ]; then
        test_name=$(basename "$test_file" .scm)
        run_test "Test: $test_name" "$test_file"
    fi
done

# 結果サマリー
echo "========================================="
echo "Test Results Summary"
echo "========================================="
echo "Total:  $TOTAL"
echo "Passed: $PASS ✅"
echo "Failed: $FAIL ❌"
echo ""

if [ $FAIL -eq 0 ]; then
    echo "🎉 All tests passed!"
    exit 0
else
    echo "⚠️  Some tests failed"
    exit 1
fi
