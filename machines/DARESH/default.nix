{
  system = "aarch64-darwin";
  homeDirectory = "/Users/d4hines";
  username = "d4hines";
  configuration = { pkgs, ... }: {
    home.packages = [ ];
    programs.gpg = {
      enable = true;
      scdaemonSettings = { disable-ccid = true; };
    };
    home.file.".gnupg/gpg-agent.conf".text = builtins.readFile ./gpg-agent.conf;
    programs.zsh = {
      enable = true;
      initExtra = ''
          export GPG_TTY="$(tty)"
          export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
          gpgconf --launch gpg-agent
        '';
    };
  };
  extraModules = [ ../../modules/home ];
}
