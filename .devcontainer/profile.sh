#!/bin/bash

alias ni="touch"
alias dtr="dotnet tool restore"

function format-changed() {
    # Get the root of the current git repository
    repo_root=$(git rev-parse --show-toplevel)

    # Run git status --porcelain in the root directory and save the output as an array
    mapfile -t status_array < <(git -C "$repo_root" status --porcelain)

    # Filter the file names in the status_array array
    filtered_files=()
    for file in "${status_array[@]}"; do
    if [[ $file =~ \.fs$ || $file =~ \.fsi$ || $file =~ \.fsx$ ]]; then
        # Trim leading symbols before the first space
        trimmed_file="${file##* }"
        filtered_files+=("$trimmed_file")
    fi
    done

    # Concatenate the filtered file names into a single string with spaces
    concatenated_files="${filtered_files[*]}"

    pushd "$repo_root" > /dev/null || exit 1

    # Run fantomas
    echo "dotnet fantomas $concatenated_files"
    dotnet fantomas $concatenated_files

    popd > /dev/null || exit
}