{ pkgs, config, ... }:
let
  # Path to forge scripts and commands in this repo
  forgeDir = ./forge;
  templatePath = "${config.home.homeDirectory}/.claude/forge-template.md";
in
{
  programs.claude-code = {
    enable = true;

    # Commands are installed to ~/.claude/commands/
    # Can be either inline string or path to file
    commands = {
      "forge-loop" = forgeDir + "/forge-loop.md";
      "cancel-forge" = forgeDir + "/cancel-forge.md";
    };

    # Settings merged into ~/.claude/settings.json
    settings = {
      hooks = {
        Stop = [
          {
            hooks = [
              {
                type = "command";
                command = "${config.home.homeDirectory}/.claude/hooks/stop-hook.ts";
              }
            ];
          }
        ];
      };
    };
  };

  # Install hook (TypeScript with bun shebang - runs directly)
  home.file.".claude/hooks/stop-hook.ts" = {
    source = forgeDir + "/stop-hook.ts";
    executable = true;
  };

  # Install forge template to known location
  home.file.".claude/forge-template.md" = {
    source = forgeDir + "/forge-template.md";
  };

  # Install setup script (TypeScript with bun shebang)
  home.file.".claude/forge-setup.ts" = {
    source = forgeDir + "/setup-forge-loop.ts";
    executable = true;
  };

  # Wrapper script that sets the template path env var
  home.packages = [
    (pkgs.writeShellScriptBin "forge-setup" ''
      export FORGE_TEMPLATE_FILE="${templatePath}"
      exec ${config.home.homeDirectory}/.claude/forge-setup.ts "$@"
    '')
  ];
}
