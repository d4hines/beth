#!/usr/bin/env bash

echo "
fn main() {}

pub struct Solution {}
" > /tmp/leetcode_template.rs

leetcode_path="$HOME/.local/share/nvim/leetcode"

rust_file="$leetcode_path/$(ls $leetcode_path | fzf)"

watchexec -c -w "$rust_file" -- "cat /tmp/leetcode_template.rs > /tmp/leetcode.rs && cat $rust_file >> /tmp/leetcode.rs && rustc /tmp/leetcode.rs && echo \"compiled successfully\""

