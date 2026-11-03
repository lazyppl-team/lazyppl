#!/bin/bash
# Plain-text snapshots of the example pages, in test/golden/.
#   bash pandoc/golden.sh check    fail when test/golden differs from freshly generated snapshots:
#                                  a changed snapshot, a missing one, or one with no page
#   bash pandoc/golden.sh update   rewrite test/golden from the pages
# pandoc 3.12.1 wrote the committed snapshots; another version can change them.
set -euo pipefail
cd "$(dirname "$0")/.."

generate() {
    for f in examples/models/*.lhs; do
        pandoc --defaults=pandoc/defaults.yaml -t plain --wrap=none --template=pandoc/golden.template "$f" -o "$1/$(basename "$f" .lhs).txt"
    done
}

case "${1:-}" in
    check)
        if grep -inF 'anonymous.4open.science' examples/models/*.lhs; then
            echo 'Example pages must not link to the anonymous review mirror.' >&2
            exit 1
        else
            status=$?
            if [ "$status" -ne 1 ]; then exit "$status"; fi
        fi
        baseline_status=$(git status --porcelain --ignored --untracked-files=all -- test/golden)
        if [ -n "$baseline_status" ]; then
            echo 'Golden snapshots must be committed and unchanged:' >&2
            printf '%s\n' "$baseline_status" >&2
            exit 1
        fi
        fresh=$(mktemp -d)
        trap 'rm -rf "$fresh"' EXIT
        generate "$fresh"
        diff -ru test/golden "$fresh"
        ;;
    update)
        rm -f test/golden/*.txt
        generate test/golden
        ;;
    *)
        echo "usage: bash pandoc/golden.sh check|update" >&2
        exit 2
        ;;
esac
