{
  rev,
  pkgs,
}: [
  {
    home.stateVersion = "21.11";
    home.homeDirectory = "/Users/d4hines";
    home.username = "d4hines";

    home.packages = with pkgs; [
      toolbox
      deploy-rs.deploy-rs
    ];
    home.file.".zshextra".text = ''
      eval "$(/opt/homebrew/bin/brew shellenv)"
      export TERM=xterm-kitty
    '';
    programs.gpg = {
      enable = true;
      scdaemonSettings = {disable-ccid = true;};
    };
    home.file.".gnupg/gpg-agent.conf".text = builtins.readFile ./gpg-agent.conf;
    home.file.".config/revision".text = "${rev}";
  }
  ../../modules/home
]
