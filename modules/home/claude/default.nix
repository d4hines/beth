{ config, ... }:
{
  programs.claude-code = {
    enable = true;

    # Settings merged into ~/.claude/settings.json
    settings = {
      permissions = {
        allow = [
          "Edit(${config.home.homeDirectory}/repos/**)"
        ];
      };
    };
  };
}
