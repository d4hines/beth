{ pkgs, ... }:
{
  home.packages =
    with pkgs; [
      vscode-fhs
      kitty
      nodejs

      fira-code
      nerdfonts
      dejavu_fonts
    ];

  fonts.fontconfig.enable = true;
  home.file.".xinitrc" = {
    text = ''
      [ -f ~/.xprofile ] && . ~/.xprofile
      exec ${pkgs.haskellPackages.xmonad}/bin/xmonad
    '';
    executable = true;
  };
  home.file.".xmobarrc".text = (import ./xmobar.nix) { isNixOS = true; };
}
