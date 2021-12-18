{ pkgs, ... }:
let
  homeDirectory = "/home/d4hines";
  username = "d4hines";
  theme =

    {
      PLAIN_COLOR = "#abb2bf";
      GREY_COLOR = "#282c34";
      DARK_GREY_COLOR = "#21252B";
      CARET_COLOR = "#528bff";
      PINK_COLOR = "#e06c75";
      CYAN_COLOR = "#56b6c2";
      LIME_COLOR = "#98c379";
      PURPLE_COLOR = "#c678dd";
      BROWN_COLOR = "#d19a66";
      GOLD_COLOR = "#e5c07b";
      BLUE_COLOR = "#61afef";
      COMMENT_COLOR = "#5c6370";
    };
in
{
  home.packages =
    with pkgs; [
      yarn
      openssh
      perf-tools
      jq
      neofetch
      watchexec
      cloc
      pandoc
      my-nodejs
      wget
      docker-compose
      fzf
      stgit
      git-crypt
      # nixos-install-tools

      ligo
      poetry

      #tmux
      #tmuxinator

      # haskellPackages.my-xmonad # includes my-xmobar
      kitty
      playerctl
      xclip
      signal-desktop
      dmenu
      dunst
      pastel
      graphviz
      my-google-chrome

      rnix-lsp
      nixpkgs-fmt

      zoom
      zotero

      fira-code
      nerdfonts
      dejavu_fonts
  ];
  programs.vim = {
    enable = true;
  };

  programs.git = {
    enable = true;
    userName = "Daniel Hines";
    userEmail = "d4hines@gmail.com";
  };
  programs.gpg.enable = true;
  services.gpg-agent.enable = true;
  services.gpg-agent.enableScDaemon = true;
  services.gpg-agent.enableSshSupport = true;
  services.gpg-agent.defaultCacheTtl = 60;
  services.gpg-agent.maxCacheTtl = 120;
  services.gpg-agent.sshKeys = [ "0x26D64B46D60FE2BB" ];
   
  # home.file.".ssh/id_rsa" = {
  #   text = builtins.readFile ./secrets/id_rsa;
  #   onChange = "sudo chmod 700 ~/.ssh/id_rsa";
  # };
  # home.file.".ssh/id_rsa.pub" = {
  #   text = builtins.readFile ./keys/id_rsa.pub;
  #   onChange = "sudo chmod 644 ~/.ssh/id_rsa.pub";
  # };
  # home.file.".ssh/authorized_keys" = {
  #   text = builtins.readFile ./keys/authorized_keys;
  #   onChange = "sudo chmod 600 ~/.ssh/authorized_keys";
  # };

  fonts.fontconfig.enable = true;

  # # https://nix-community.github.io/home-manager/options.html#opt-nixpkgs.config
  # # nixpkgs.config = { allowBroken = true; } 

  # home.sessionVariables = theme // {
  #   BROWSER = "chrome";
  #   EDITOR = "vim";
  #   COMPLICE_TOKEN = builtins.readFile ./secrets/complice_api;
  #   # ROAM_CREDENTIALS = builtins.readFile ./secrets/roam_credentials;
  #   DEFAULT_USER = username; # for agnoster oh-my-zsh theme.
  #   TEZOS_DIR = "${homeDirectory}/repos/tezos";
  #   OCAMLRUNPARAM = "b";
  # };

  # programs.home-manager.enable = true;
  # # I use Zsh for my shell but it's good to have bash around
  # programs.bash.enable = true;
  programs.zsh.enable = true;
  # # TODO: add missing zsh init config
  # programs.zsh.shellAliases = {
  #   # Only requires flakes-enabled nix and for this repo
  #   # to be at path ~/repos/beth. (i.e works even if
  #   # home-hanager isn't installed yet.)
  #   # You can install nix with the nix-flakes-installer, e.g:
  #   # sh <(curl -L https://github.com/numtide/nix-flakes-installer/releases/download/nix-2.4pre20210604_8e6ee1b/install)
  #   home_reload =
  #     (
  #       let
  #         original = "home-manager switch --flake .#d4hines";
  #         nix-version = "nix run github:nix-community/home-manager --no-write-lock-file -- switch --flake .#d4hines";
  #       in
  #       "(cd ~/repos/beth && which home-manager && ${original} || ${nix-version})"
  #     );
  #   save_config = "(cd ~/repos/beth/aconfmgr && ./aconfmgr save -c ../arch_config)";
  #   icat = "kitty +kitten icat";
  #   fzf_preview = ''fzf --preview "preview {}" --preview-window left:40%'';
  #   watchexec = "watchexec --shell='bash --login -O expand_aliases'";
  #   # Tezos specific stuff
  #   cdp = "cd $TEZOS_DIR/src/proto_alpha/lib_protocol";
  #   cdt = "cd $TEZOS_DIR";
  #   cdu = "cd $TEZOS_DIR/src/proto_alpha/lib_protocol/test/unit";
  #   turn_off_warnings = ''export OCAMLPARAM="_,w=-27-26-32-33-20-21-37-34"'';
  #   runtest = "dune build --terminal-persistence=clear-on-rebuild  @runtest_proto_alpha --watch";
  #   test_globals = ''(cdu && dune build @runtest --force ) && dune exec ./src/proto_alpha/lib_protocol/test/main.exe -- test "global table of constants" -c && tezt global_constant'';
  #   dbw = "dune build --terminal-persistence=clear-on-rebuild --watch";
  #   tezt = ''dune exec tezt/tests/main.exe --'';
  #   destroy_mockup = "rm -rf /tmp/mockup";
  #   mockup_client = "create_mockup && tezos-client --mode mockup --base-dir /tmp/mockup";
  #   client = "mockup_client";
  # };
  # xsession.enable = true;
  # xsession.windowManager.command = "my-xmonad";
  # xsession.initExtra = ''
  #   export LANG=en_US.UTF-8
  #   ~/scripts/complice.js | my-xmobar &
  #   ~/scripts/browser_whitelist.js &
  # '';
  # home.file.".xinitrc" = {
  #   # Because home-manager puts a lot of settijngs in .xsession,
  #   # all we do in .xinit is call .xsession.
  #   text = ''#!/bin/sh
  #   . ~/.xsession
  #   '';
  #   executable = true;
  # };
}
