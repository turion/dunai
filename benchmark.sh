#!/bin/sh

set -e


commits=$*

for commit in $commits; do
    if ! git cat-file -t $commit &>/dev/null; then
        echo >&2 "Could not find revision $commit"
        exit 2
    fi
done

original_commit=`git rev-parse --abbrev-ref HEAD`

run_benchmark () {
    cabal run spaceleak --enable-profiling  -- +RTS -sstderr
}

benchmark() {
    time_result=$(run_benchmark 2>&1 1>&2)
    echo $time_result
    echo $time_result | grep "bytes copied"
    gc_bytes=$(echo $time_result | grep "bytes copied" | awk '{print $1;}')
    allocated=$(echo $time_result | grep "bytes allocated" | awk '{print $1;}')
    mut=$(echo $time_result | grep "MUT" | awk '{print $3;}')
    gc=$(echo $time_result | grep "GC" | awk '{print $3;}')
    # echo "| $time_result"
    echo "| $gc_bytes | $allocated | $mut | $gc |"
}

echo "| commit | hash | msg | gc_bytes | allocated | mut | gc |"
echo "| ------ | ---- | --- | -------- | --------- | --- | -- |"
for commit in $commits; do
    git checkout "$commit" &>/dev/null
    echo -n "| $commit | $(git log --format="%H | %s" -n 1) "
    if ! benchmark &>/dev/null; then
        echo >&2 "Execution failed"
        exit 1
    fi
    benchmark
done

git checkout "$original_commit"
