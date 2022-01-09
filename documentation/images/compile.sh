#!/bin/bash

# src: https://j2r2b.github.io/2019/08/06/drawio-cli.html
# requires the installation of command-line program 'drawio'
# => see https://github.com/jgraph/drawio-desktop

current_file="$0"
current_dir="${current_file%compile.sh}"
echo
drawio -x --crop -f pdf -o "$current_dir"/out/ "$current_dir"/source/