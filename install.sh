#!/bin/sh

CommandAvailable ()
{
    [ -n "$1" ] && command -v "$1" 2>&1 > /dev/null
}

if ! CommandAvailable stow; then
    echo "This script requires GNU Stow to work."
    exit 1
fi

for program in *; do

    # skip top level files (README.md, etc.)
    [ ! -d "$program" ] && continue

    CommandAvailable "$program" || echo "$program not installed; skipping"

    stow "$program" 2>/dev/null
done
