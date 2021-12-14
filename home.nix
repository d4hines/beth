{ pkgs, ... }:
{
  programs.vim = {
    enable = true;
  };

  programs.git = {
    enable = true;
    userName = "Daniel Hines";
    userEmail = "d4hines@gmail.com";
  };
}
