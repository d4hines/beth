{ pkgs, ... }:
let
  theme = import ./theme.nix;
in
{
  home.stateVersion = "21.11";
  home.packages =
    with pkgs; [
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
      bat
      lsof
      gdb
      comby
      socat
      shellcheck
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
      # neovim
      # google-cloud-sdk
      rage
      # gke-gcloud-auth-plugin
      kubectl
      neofetch
      
      exercism
      fira-code
      nerdfonts
      dejavu_fonts

      rnix-lsp
      nixfmt
      haskellPackages.nix-derivation
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

  home.sessionVariables = theme // {
    EDITOR = "vim";
    DEFAULT_USER = "d4hines"; # for agnoster oh-my-zsh theme.
    OCAMLRUNPARAM = "b";
    GHSTACK_OAUTH_TOKEN = builtins.readFile ../../secrets/gh_token;
  };

  programs.home-manager.enable = true;
  # I use Zsh for my shell but it's good to have bash around
  programs.bash.enable = true;
  programs.zsh.enable = true;
  programs.zsh.initExtra =
    ''
      #TODO: why is this necessary when I already set??
      export EDITOR="nvim";

      # TODO: seems like home.sessionPath shoudl work but doesn't??
      export PATH=~/.npm-global/bin:~/repos/helix/result/bin:$PATH
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
    # Tezos specific stuff
    cdp = "cd $TEZOS_DIR/src/proto_alpha/lib_protocol";
    cdt = "cd $TEZOS_DIR";
    cdu = "cd $TEZOS_DIR/src/proto_alpha/lib_protocol/test/unit";
    anger = "~/repos/anger/_build/install/default/bin/anger";
    cat = "bat";
  };
  programs.zsh.oh-my-zsh.enable = true;
  programs.zsh.oh-my-zsh.theme = "agnoster";

  home.file.".config/kitty/kitty.conf".text = ''
    font_family      Fira Code
    bold_font        Fira Code Bold
    italic_font      auto
    bold_italic_font auto

    font_size 12

    enabled_layouts tall:bias=50;full_size=1;mirrored=false

    ${builtins.readFile ./one_dark.conf}
  '';

  # Run on interactive shells
  programs.direnv.enable = true;
  programs.direnv.enableZshIntegration = true;
  programs.direnv.nix-direnv.enable = true;

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
      diff = {
        tool = "difft";
        external = "difft";
        colorMoved = "default";
      };
      difftool.prompt = false;
      difftool.difft.cmd = "difft \"$LOCAL\" $\"$REMOTE\"";
      core.pager = "delta";
      pager.difftool = true;
      interactive.diffFilter = "detla --color-only";
      delta = {
        features = "side-by-side line-numbers decorations";
      };
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

  home.file.".config/exercism/user.json".text = ''
    {
     "apibaseurl": "https://api.exercism.io/v1",
     "token": "${builtins.readFile ../../secrets/exercism}",
     "workspace": "/home/d4hines/repos/exercism"
     }'';
  fonts.fontconfig.enable = true;
  home.file.".config/lazygit/config.yml".text = builtins.readFile ./lazy_git_config.yml;
  home.file.".config/helix/config.toml".text = builtins.readFile ./helix_config.yml;
  home.file.".obs_scene_change.lua".text = builtins.readFile ./obs_scene_change.lua;

  programs.tmux.enable = true;
}
