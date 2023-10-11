final: prev: let
  pkgs =
    prev
    // {
      gitconfig = prev.writeTextDir "share/.gitconfig" (builtins.readFile ./.gitconfig);
      direnvrc = pkgs.writeTextDir "share/direnv/direnvrc" ''
        source ${pkgs.nix-direnv}/share/nix-direnv/direnvrc
      '';
      gituiconfig = pkgs.symlinkJoin {
        name = "gituiconfig";
        paths = [
          (pkgs.writeTextDir
            "share/gitui/key_bindings.ron"
            (builtins.readFile ./gitui_key_bindings.ron))
        ];
      };
      tmuxconfig = prev.writeTextDir "share/tmux.conf" (builtins.readFile ./tmux.conf);
    };
  sub_packages = packages: text:
    builtins.foldl'
    (
      acc: package:
        builtins.replaceStrings
        [("$$$" + package)]
        ["${pkgs.${package}}"]
        acc
    )
    text
    packages;
  zshconfig = pkgs.symlinkJoin {
    name = "zshconfig";
    paths = [
      (
        pkgs.writeTextDir "share/.zshenv"
        (builtins.readFile ./.zshenv)
      )
      (
        pkgs.writeTextDir "share/.zshrc"
        (sub_packages [
            "oh-my-zsh"
            "zoxide"
            "direnv"
            "gitconfig"
            "gituiconfig"
            "tmuxconfig"
          ]
          (builtins.readFile ./.zshrc))
      )
    ];
  };
  runtimeInputs = with pkgs; [
    binutils
    jq
    cloc
    wget
    fzf
    file
    lsof
    socat
    time
    gitui
    ripgrep
    watchexec
    tmux
    nodejs
    eza
    htop
    gitui
    vim

    zoxide
    direnv
  ];
in {
  toolbox =
    (pkgs.writeScriptBin "toolbox" ''
      #!${pkgs.zsh}/bin/zsh
      export PATH="${final.lib.makeBinPath runtimeInputs}:$PATH"
      export SHELL=${pkgs.zsh}/bin/zsh

      mkdir -p /tmp/zshdotdir
      ln -f -s ${zshconfig}/share/.zshrc /tmp/zshdotdir
      ln -f -s ${zshconfig}/share/.zshenv /tmp/zshdotdir

      ZDOTDIR=/tmp/zshdotdir exec ${pkgs.zsh}/bin/zsh "$@"
    '')
    .overrideAttrs (_: {shellPath = "/bin/toolbox";});
}
