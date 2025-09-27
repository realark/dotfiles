#!/usr/bin/env bash

SCRIPT_DIRECTORY="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
cd "$SCRIPT_DIRECTORY" || exit 1

# Treat arguments as an array
backups=("$@")

# echo "-- backups? ${backups[*]} --"

# Keep a few of the most recent
filesToKeep=("${backups[@]: -3}")

# echo "-- keep? ${filesToKeep[*]} --"

for file in "${backups[@]}"; do
    keep=false
    for keepFile in "${filesToKeep[@]}"; do
        if [[ "$file" == "$keepFile" ]]; then
            keep=true
            break
        fi
    done

    if $keep; then
        echo "Keeping recent backup: $file"
    else
        echo "Removing old backup: $file"
        rm -f "$file"
    fi
done
