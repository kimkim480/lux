#!/bin/bash

cargo build --quiet
if [ $? -ne 0 ]; then
    echo "❌ Build failed."
    exit 1
fi

bin="./target/debug/lux"

EXIT_CODE=0

TOTAL=0
PASSED=0
FAILED=0

FAILED_FILES=()

run_lux_file() {
    local file="$1"
    ((TOTAL++))
    echo "▶ Running: $file"

    OUTPUT=$($bin run "$file" 2>&1)
    STATUS=$?

    if [[ $STATUS -ne 0 || "$OUTPUT" == *"error:"* || "$OUTPUT" == *"Syntax error"* || "$OUTPUT" == *"Compile error"* || "$OUTPUT" == *"Runtime error"* ]]; then
        echo "❌ Error in $file:"
        echo "$OUTPUT"
        echo "--------------------------------------------------"
        FAILED_FILES+=("$file")
        ((FAILED++))
        EXIT_CODE=1
    else
        echo "✅ Passed: $file"
        ((PASSED++))
    fi
}

export -f run_lux_file
export LUX_EXEC
export -n EXIT_CODE # prevent exporting this one
export -n TOTAL PASSED FAILED # not needed in subshell

FILES=$(find algos test -name "*.lux")

while IFS= read -r file; do
    run_lux_file "$file"
done <<< "$FILES"

# Summary
echo
echo "================== SUMMARY =================="
echo "Total files: $TOTAL"
echo "Passed:      $PASSED"
echo "Failed:      $FAILED"

if (( FAILED > 0 )); then
    echo
    echo "❌ Failed files:"
    for f in "${FAILED_FILES[@]}"; do
        echo " - $f"
    done
fi

exit $EXIT_CODE
