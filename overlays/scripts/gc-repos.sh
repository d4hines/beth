# gc-repos: interactively push-then-delete git repos in a directory.
# Usage: gc-repos [dir]   (defaults to the current directory)

root="$(cd "${1:-.}" && pwd)"

red=$'\e[31m'
green=$'\e[32m'
yellow=$'\e[33m'
dim=$'\e[2m'
reset=$'\e[0m'

is_dirty() { [ -n "$(git -C "$1" status --porcelain 2>/dev/null)" ]; }
has_remote() { [ -n "$(git -C "$1" remote)" ]; }
# Commits on any local branch that aren't on any remote-tracking branch
# (as of the last fetch).
unpushed_count() { git -C "$1" rev-list --count --branches --not --remotes; }

# One line per repo: "<name>\t<display>". fzf shows only the display column.
list_repos() {
  local d name branch state remote width=0
  local -a names=()
  for d in "$root"/*/; do
    d="${d%/}"
    [ -e "$d/.git" ] || continue
    name="$(basename "$d")"
    names+=("$name")
    ((${#name} > width)) && width=${#name}
  done
  for name in "${names[@]}"; do
    d="$root/$name"
    branch="$(git -C "$d" symbolic-ref --short -q HEAD || echo "(detached)")"
    if is_dirty "$d"; then state="${red}dirty${reset}"; else state="${green}clean${reset}"; fi
    if ! has_remote "$d"; then
      remote="${red}no remote${reset}"
    else
      local n
      n="$(unpushed_count "$d")"
      if ((n > 0)); then remote="${yellow}${n} unpushed${reset}"; else remote="${green}pushed${reset}"; fi
    fi
    printf '%s\t%-*s  %s  %-22s %s\n' "$name" "$width" "$name" "$state" "$remote" "${dim}${branch}${reset}"
  done
}

# Push every local branch that has commits not on any remote.
push_unpushed() {
  local d="$1" remote branch
  if git -C "$d" remote | grep -qx origin; then remote=origin; else remote="$(git -C "$d" remote | head -n1)"; fi
  while read -r branch; do
    if (($(git -C "$d" rev-list --count "refs/heads/$branch" --not --remotes) > 0)); then
      git -C "$d" push -u "$remote" "$branch" || return 1
    fi
  done < <(git -C "$d" for-each-ref --format='%(refname:short)' refs/heads)
}

# Push anything unpushed, then delete. A repo is only deleted once nothing
# committed is left unpushed.
gc_repos() {
  local name d n
  local -a ok=()
  for name in "$@"; do
    d="$root/$name"
    if ! has_remote "$d"; then
      echo "${red}✗ $name: no remote, skipping${reset}"
      continue
    fi
    is_dirty "$d" && echo "${yellow}! $name: uncommitted changes will be lost${reset}"
    n="$(unpushed_count "$d")"
    ((n > 0)) && echo "${dim}  $name: will push $n unpushed commit(s) first${reset}"
    ok+=("$name")
  done
  ((${#ok[@]} == 0)) && return 1
  echo
  echo "About to push & delete: ${ok[*]}"
  read -rp "Type 'yes' to confirm: " answer
  if [ "$answer" != yes ]; then
    echo "Aborted."
    return 0
  fi
  for name in "${ok[@]}"; do
    d="$root/$name"
    echo "── $name"
    if ! push_unpushed "$d" || (($(unpushed_count "$d") > 0)); then
      echo "${red}✗ $name: push failed, not deleting${reset}"
      continue
    fi
    rm -rf "${root:?}/$name" && echo "${green}✓ deleted $name${reset}"
  done
}

preview="git -C $(printf %q "$root")/{1} -c color.status=always status -sb; echo; git -C $(printf %q "$root")/{1} log --oneline --graph --color=always -20"

while true; do
  repos="$(list_repos)"
  if [ -z "$repos" ]; then
    echo "No git repos in $root"
    exit 0
  fi
  out="$(fzf <<<"$repos" --ansi --multi --delimiter $'\t' --with-nth 2 \
    --header "$root  ·  tab: select  ·  enter: push & delete  ·  esc: quit" \
    --preview "$preview" --preview-window right,50%)" || exit 0

  mapfile -t selected < <(cut -f1 <<<"$out")
  gc_repos "${selected[@]}" || true
  echo
  read -rp "Press enter to continue..." _
done
