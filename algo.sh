#!/bin/bash

cargo build --quiet
if [ $? -ne 0 ]; then
    echo "❌ Build failed."
    exit 1
fi

bin="./target/debug/lux"

for file in $(find algos -name "*.lux"); do
    echo "🧪 Testing Algos $file"

    output=$($bin "$file" 2>&1 | sed 's/[[:space:]]*$//')
    echo "$output"
done