#!/bin/bash

pass=0
fail=0

cargo build --quiet
if [ $? -ne 0 ]; then
    echo "❌ Build failed."
    exit 1
fi

bin="./target/debug/lux"

for file in $(find test -name "*.lux"); do
    echo "🧪 Testing $file"

    output=$($bin "$file" 2>&1 | sed 's/[[:space:]]*$//')
    readarray -t actual_lines <<< "$output"

    # Get expected values from lines like: // test: <Light(8.0)>
    readarray -t expected_lines < <(grep "// test:" "$file" | sed -n 's/.*test: <\(.*\)>/\1/p')

    mismatch=0
    for i in "${!expected_lines[@]}"; do
        expected="${expected_lines[$i]}"
        actual="${actual_lines[$i]:-}"

        if [[ "$expected" != "$actual" ]]; then
            echo "❌ Mismatch at line $((i+1))"
            echo "   Expected: $expected"
            echo "   Got:      $actual"
            mismatch=1
        fi
    done

    if [[ $mismatch -eq 0 ]]; then
        echo "✅ Passed"
        ((pass++))
    else
        echo "❌ Failed"
        ((fail++))
    fi
done

echo ""
echo "📊 Test summary:"
echo "✅ Passed: $pass"
echo "❌ Failed: $fail"
