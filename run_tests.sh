#!/bin/bash
# Test runner script

echo "===================="
echo "Scheme Interpreter Test Suite"
echo "===================="

TESTS_DIR="tests"
INTERPRETER="cabal run scheme-interpreter"

PASSED=0
FAILED=0

# Run each test file
for test_file in $TESTS_DIR/*.scm; do
    echo ""
    echo "Running $(basename $test_file)..."
    
    if timeout 5 $INTERPRETER < "$test_file" 2>&1; then
        PASSED=$((PASSED + 1))
    else
        echo "FAILED: $test_file"
        FAILED=$((FAILED + 1))
    fi
done

echo ""
echo "===================="
echo "Test Results:"
echo "  Passed: $PASSED"
echo "  Failed: $FAILED"
echo "===================="

if [ $FAILED -eq 0 ]; then
    echo "✅ All tests passed!"
    exit 0
else
    echo "❌ Some tests failed"
    exit 1
fi
