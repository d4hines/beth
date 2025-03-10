# Adapted with gratitude from https://github.com/kevinywlui/zlong_alert.zsh

# Use zsh/datetime for $EPOCHSECONDS
zmodload zsh/datetime || return

# Be sure we can actually set hooks
autoload -Uz add-zsh-hook || return

# Set as true to enable terminal bell (beep)
zlong_terminal_bell='true'

# Define a long duration if needed
zlong_duration=15

# Set commands to ignore (do not notify) if needed
zlong_ignore_cmds='vim ssh vim nvim nix-shell tmux orb'

# Set prefixes to ignore (consider command in argument) if needed
zlong_ignore_pfxs='sudo time'

# Set as true to ignore commands starting with a space
zlong_ignorespace='false'

# Define a custom message to display
zlong_message='"Done: $cmd Time: $ftime"'

# Define the alerting function, do the text processing here
zlong_alert_func() {
    local cmd="$1"
    local secs="$2"
    local ftime="$(printf '%dh:%dm:%ds\n' $(($secs / 3600)) $(($secs % 3600 / 60)) $(($secs % 60)))"
    if command -v mac >/dev/null 2>&1; then
        # From Orb
        mac terminal-notifier -title "Done: $cmd" -message "Time: $ftime" 
        mac "afplay ~/done.wav"
    elif command -v afplay >/dev/null 2>&1; then
        # From Mac
        terminal-notifier -title "Done: $cmd" -message "Time: $ftime" 
        afplay ~/done.wav
    elif command -v notify-send >/dev/null 2>&1; then
        # From Linux
        notify-send -h string:bgcolor:#e5c07b-h "Done: $cmd" "Time: $ftime"
        paplay "$DONE_WAV"
    fi
    echo "$cmd,$secs" >> ~/.zsh_long_command_history # let's keep track of which command take the longest
}

zlong_alert_pre() {
    zlong_last_cmd="$1"

    if [[ "$zlong_ignorespace" == 'true' && "${zlong_last_cmd:0:1}" == [[:space:]] ]]; then
        # set internal variables to nothing ignoring this command
        zlong_last_cmd=''
        zlong_timestamp=0
    else
        zlong_timestamp=$EPOCHSECONDS
    fi
}

zlong_alert_post() {
    local duration=$(($EPOCHSECONDS - ${zlong_timestamp-$EPOCHSECONDS}))
    local lasted_long=$(($duration - $zlong_duration))
    local cmd_head

    # Ignore leading spaces (-L) and command prefixes (like time and sudo)
    typeset -L last_cmd_no_pfx="$zlong_last_cmd"
    local no_pfx
    while [[ -n "$last_cmd_no_pfx" && -z "$no_pfx" ]]; do
 	cmd_head="${last_cmd_no_pfx%% *}"
	if [[ "$zlong_ignore_pfxs" =~ "(^|[[:space:]])${(q)cmd_head}([[:space:]]|$)" ]]; then
	    last_cmd_no_pfx="${last_cmd_no_pfx#* }"
	else
	    no_pfx=true
	fi
    done

    # Notify only if delay > $zlong_duration and command not ignored
    if [[ $lasted_long -gt 0 && ! -z $last_cmd_no_pfx && ! "$zlong_ignore_cmds" =~ "(^|[[:space:]])${(q)cmd_head}([[:space:]]|$)" ]]; then
        zlong_alert_func "$zlong_last_cmd" "$duration"
    fi
    zlong_last_cmd=''
}

add-zsh-hook preexec zlong_alert_pre
add-zsh-hook precmd zlong_alert_post
