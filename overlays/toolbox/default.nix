final: prev: let
  pkgs =
    prev
    // {
      # gitconfig = prev.writeTextDir "share/.gitconfig" (builtins.readFile ./.gitconfig);
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
        (sub_packages [
            "zsh"
          ] (
            ''export PATH=${final.lib.makeBinPath runtimeInputs}:$PATH''
            + "\n"
            + builtins.readFile ./.zshenv
          ))
      )
      (
        pkgs.writeTextDir "share/.zshrc"
        (sub_packages [
            "oh-my-zsh"
            "zoxide"
            "direnv"
            #"gitconfig"
            "gituiconfig"
            "tmuxconfig"
          ]
          (builtins.readFile ./.zshrc))
      )
    ];
  };
  runtimeInputs = with pkgs; [
    zsh
    nix
    #gcc
    rsync
    #binutils
    jq
    cloc
    wget
    netcat
    fzf
    file
    lsof
    socat
    time
    gitui
    ripgrep
    watchexec
    tmux
    #nodejs
    eza
    htop
    gitui
    vim
    man

    zoxide
    direnv

    # My scripts
    final.wta
  ];
in {
  toolbox =
    (pkgs.writeScriptBin "toolbox" ''
      #!${pkgs.zsh}/bin/zsh

      mkdir -p /tmp/zshdotdir
      ln -f -s ${zshconfig}/share/.zshrc /tmp/zshdotdir
      ln -f -s ${zshconfig}/share/.zshenv /tmp/zshdotdir

      export ZDOTDIR=/tmp/zshdotdir

      # Fixes utf8 chars on non-NixOS Linux
      if [[ -e "/usr/lib/locale/locale-archive" ]]; then
        export LOCALE_ARCHIVE="/usr/lib/locale/locale-archive"
      fi

      exec ${pkgs.zsh}/bin/zsh -i "$@"
    '')
    .overrideAttrs (_: {shellPath = "/bin/toolbox";});
}
