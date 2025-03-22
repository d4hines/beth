{ gitUserName, gitUserEmail }:
{
  pkgs,
  config,
  lib,
  ...
}:
{
  home.sessionVariables = {
    EDITOR = "vim";
    OCAMLRUNPARAM = "b";
    RUST_BACKTRACE = "1";
    DUNE_CONFIG__GLOBAL_LOCK = "disabled";
  };
  programs.zsh = {
    enable = true;
    shellAliases = {
      fzf_preview = "fzf --preview \"preview {}\" --preview-window left:40%";
      icat = "kitty +kitten icat";
      turn_off_warnings = "export OCAMLPARAM=\"_,w=-27-26-32-33-20-21-37-34\"";
      watchexec = "watchexec --shell='bash --login -O expand_aliases'";
      gcwt = "git worktree list --porcelain | grep worktree | cut -d ' ' -f 2 | fzf --multi | xargs -I {} sh -c 'echo \"Removing worktree {}\" && git worktree remove {}'";
      gc = "git commit -v";
      gca = "git commit --amend";
      gaa = "git add -A";
      gpf = "git push --force-with-lease";
      anger = "$HOME/repos/anger/result/bin/anger"; # sloppy but IDK
    };
    initExtra = ''
      bindkey "^[OB" history-beginning-search-forward
      export PATH=~/.cargo/bin:~/.npm-global/bin:~/.local/bin/:$PATH

      if [[ -e "$HOME/.zshextra" ]]; then
          source "$HOME/.zshextra"
      fi
      if [ -z "$TMUX" ] && [ "$TERM" = "xterm-kitty" ]; then
        tmux attach || exec tmux new-session;
      fi 
      export TERM=xterm-256color

      #### Zlong alert ####
      # Plays an alert for long-running commands
      DONE_WAV=${./done.wav}
      ${builtins.readFile ../../overlays/scripts/zlong_alert.zsh}
      #####################
    '';
    oh-my-zsh = {
      enable = true;
      theme = "agnoster";
    };
    history = {
      size = 1000000000;
      save = 1000000000;
      ignoreDups = true;
      ignoreSpace = true;
      share = true;
    };
  };

  home.enableNixpkgsReleaseCheck = true;
  home.packages = with pkgs; [
    nix-tree
    # patdiff
    yarn
    yubikey-manager
    age-plugin-yubikey
    log-hours
    # go
    # gopls
    # ffmpeg
    bun
    zip
    unzip
    dust
    # fira-code
    # nerd-fonts.fira-code
    # dejavu_fonts
    nil
    cloudflared
    nixfmt-rfc-style
    # code2prompt
    #### toolboxy things  ####
    rsync
    jq
    cloc
    wget
    netcat
    fzf
    file
    lsof
    socat
    time
    ripgrep
    watchexec
    tmux
    eza
    ##### My scripts #####
    wta
  ];
  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };
  programs.home-manager.enable = true;
  # I use Zsh for my shell but it's good to have bash around
  programs.bash.enable = true;
  programs.git = {
    enable = true;
    userName = gitUserName;
    userEmail = gitUserEmail;
    aliases = {
      branchname = "symbolic-ref --short -q HEAD";
      co = "checkout";
      cp = "cherry-pick";
      fixup = "!git log -n 50 --oneline --no-merges | fzf | cut -c -7 | xargs -o git commit --fixup";
    };
    extraConfig = {
      blame.ignoreRevsFile = ".git-blame-ignore-revs";
      merge.conflictStyle = "diff3";
      notes = {
        rewriteMode = "overwrite";
        rewriteRef = "refs/notes/commits";
      };
      pull.rebase = false;
      "filter \"lfs\"" = {
        clean = "git-lfs clean -- %f";
        smudge = "git-lfs smudge -- %f";
        process = "git-lfs filter-process";
        required = true;
      };
    };
  };
  programs.zoxide = {
    enable = true;
    enableZshIntegration = true;
  };

  home.file.".config/kitty/kitty.conf".text = ''
    font_family      Fira Code
    bold_font        Fira Code Bold
    italic_font      auto
    bold_italic_font auto

    copy_on_select yes

    font_size 10

    enabled_layouts tall:bias=50;full_size=1;mirrored=false

    macos_option_as_alt yes
    macos_hide_titlebar yes

    map ctrl+shift+r discard_event

    ${builtins.readFile ./catpuccin.conf}
  '';
  # fonts.fontconfig.enable = true;
  home.file.".tmux.conf".text = builtins.readFile ./tmux.conf;
  home.file."done.wav".source = ./done.wav;
}
