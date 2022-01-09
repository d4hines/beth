{ pkgs, ... }: {
  home.packages =
    with pkgs; [
      vscode-fhs
      kitty
      nodejs
    ];
}
