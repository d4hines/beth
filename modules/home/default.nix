{pkgs, ...}: let
  theme = import ./theme.nix;
in {
  home.enableNixpkgsReleaseCheck = true;
  home.stateVersion = "21.11";
  home.packages = with pkgs; [
    vim
    binutils
    yarn
    openssh
    jq
    cloc
    wget
    docker-compose
    fzf
    git-crypt
    file
    lsof
    gdb
    comby
    socat
    time
    nodejs
    gh
    gitui
    lazygit
    delta
    ripgrep
    patdiff
    difftastic
    exa
    ranger
    rage
    kubectl
    neofetch
    zellij
    zoxide
    watchexec
    tmux 
    tmuxinator 

    fira-code
    nerdfonts
    dejavu_fonts

    rnix-lsp
    nil
    alejandra
    haskellPackages.nix-derivation

    nodePackages.typescript-language-server
  ];

  home.file.".ssh/id_rsa" = {
    text = builtins.readFile ../../secrets/id_rsa;
    onChange = "sudo chmod 700 ~/.ssh/id_rsa";
  };
  home.file.".ssh/id_rsa.pub" = {
    text = builtins.readFile ../../keys/id_rsa.pub;
    onChange = "sudo chmod 644 ~/.ssh/id_rsa.pub";
  };
  home.file.".ssh/authorized_keys" = {
    text = builtins.readFile ../../keys/authorized_keys;
    onChange = "sudo chmod 600 ~/.ssh/authorized_keys";
  };

  home.sessionVariables =
    theme
    // {
      EDITOR = "vim";
      DEFAULT_USER = "d4hines"; # for agnoster oh-my-zsh theme.
      OCAMLRUNPARAM = "b";
      GHSTACK_OAUTH_TOKEN = builtins.readFile ../../secrets/gh_token;
      RUST_BACKTRACE = "1";
    };

  programs.home-manager.enable = true;
  # I use Zsh for my shell but it's good to have bash around
  programs.bash.enable = true;
  programs.zsh.enable = true;
  programs.zsh.initExtra = ''
    #TODO: why is this necessary when I already set??
    export EDITOR="hx";
    export HISTSIZE=1000000000
    export HISTFILESIZE=1000000000

    # TODO: seems like home.sessionPath shoudl work but doesn't??
    export PATH=~/.npm-global/bin:~/repos/helix/result/bin:$PATH

    # zellij hook
    if [[ -z "$ZELLIJ" && "$TERM_PROGRAM" -ne "vscode" ]]; then
      if [[ "$ZELLIJ_AUTO_ATTACH" == "true" ]]; then
        zellij attach -c
      else
        zellij
      fi
      if [[ "$ZELLIJ_AUTO_EXIT" == "true" ]]; then
        exit
      fi
    fi
  '';
  programs.zsh.shellAliases = {
    # Only requires flakes-enabled nix and for this repo
    # to be at path ~/repos/beth. (i.e works even if
    # home-hanager isn't installed yet.)
    # You can install nix with the nix-flakes-installer, e.g:
    # sh <(curl -L https://github.com/numtide/nix-flakes-installer/releases/download/nix-2.4pre20210604_8e6ee1b/install)
    icat = "kitty +kitten icat";
    fzf_preview = ''fzf --preview "preview {}" --preview-window left:40%'';
    watchexec = "watchexec --shell='bash --login -O expand_aliases'";
    scu = "systemctl --user";
    turn_off_warnings = ''export OCAMLPARAM="_,w=-27-26-32-33-20-21-37-34"'';
    anger = "~/repos/anger/_build/install/default/bin/anger";
  };
  programs.zsh.oh-my-zsh.enable = true;
  programs.zsh.oh-my-zsh.theme = "agnoster";

  home.file.".config/kitty/kitty.conf".text = ''
    font_family      Fira Code
    bold_font        Fira Code Bold
    italic_font      auto
    bold_italic_font auto

    copy_on_select yes

    font_size 12

    enabled_layouts tall:bias=50;full_size=1;mirrored=false

    macos_option_as_alt yes

    map ctrl+shift+r discard_event

    ${builtins.readFile ./catpuccin.conf}
  '';

  # Run on interactive shells
  programs.direnv = {
    enable = true;
    enableZshIntegration = true;
    nix-direnv.enable = true;
  };

  programs.git = {
    enable = true;
    userEmail = "d4hines@gmail.com";
    userName = "Daniel Hines";

    extraConfig = {
      # functionality
      pull.rebase = false;
      notes.rewriteMode = "overwrite";
      notes.rewriteRef = "refs/notes/commits";

      merge.conflictstyle = "diff3";

      # Diff viewing
      # diff = {
      #   tool = "difft";
      #   external = "difft";
      #   colorMoved = "default";
      # };
      # difftool.prompt = false;
      # difftool.difft.cmd = "difft \"$LOCAL\" $\"$REMOTE\"";
      # core.pager = "delta";
      # pager.difftool = true;
      # interactive.diffFilter = "detla --color-only";
      # delta = {
      #   features = "side-by-side line-numbers decorations";
      # };
    };

    aliases = {
      co = "checkout";
      branchname = "symbolic-ref --short -q HEAD";
      cp = "cherry-pick";
      fixup = "!git log -n 50 --oneline --no-merges | fzf | cut -c -7 | xargs -o git commit --fixup";
      clone-worktree = "!clone-bare-for-worktrees";
    };
  };

  programs.htop.enable = true;
  programs.man.enable = true;

  fonts.fontconfig.enable = true;
  # home.file.".config/lazygit/config.yml".text = builtins.readFile ./lazy_git_config.yml;
  # home.file.".config/helix/config.toml".text = builtins.readFile ./helix_config.yml;
  home.file.".obs_scene_change.lua".text = builtins.readFile ./obs_scene_change.lua;
  home.file.".config/discord/settings.json".text = ''    {
        "SKIP_HOST_UPDATE": true,
        "BACKGROUND_COLOR": "#202225",
        "IS_MAXIMIZED": false,
        "IS_MINIMIZED": false,
        "WINDOW_BOUNDS": {
          "x": 2,
          "y": 2,
          "width": 2556,
          "height": 1408
        }
      }'';
  home.file.".config/gitui/theme.ron".text = builtins.readFile ./gitui_theme.ron;
  home.file.".config/gitui/key_bindings.ron".text = builtins.readFile ./gitui_key_bindings.ron;
}
