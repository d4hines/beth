{ pkgs, config, ... }:
let
  # Path to wiggum scripts and commands in this repo
  wiggumDir = ./wiggum;
in
{
  programs.claude-code = {
    enable = true;

    # Commands are installed to ~/.claude/commands/
    # Can be either inline string or path to file
    commands = {
      "wiggum-loop" = wiggumDir + "/wiggum-loop.md";
      "cancel-wiggum" = wiggumDir + "/cancel-wiggum.md";
    };

    # Settings merged into ~/.claude/settings.json
    settings = {
      hooks = {
        Stop = [
          {
            hooks = [
              {
                type = "command";
                command = "${config.home.homeDirectory}/.claude/hooks/stop-hook.sh";
              }
            ];
          }
        ];
      };
    };
  };

  # Install hook with executable bit (home-manager hooks option doesn't set +x)
  home.file.".claude/hooks/stop-hook.sh" = {
    text = builtins.readFile (wiggumDir + "/stop-hook.sh");
    executable = true;
  };

  # Ensure the setup script is in PATH
  home.packages = [
    (pkgs.writeShellScriptBin "wiggum-setup" (builtins.readFile (wiggumDir + "/setup-wiggum-loop.sh")))
  ];
}
