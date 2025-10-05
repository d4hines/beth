{ ... }:
{
  imports = [ ../../modules/darwin ];
  system.primaryUser = "d4hines";
  environment.variables = {
    DEFAULT_USER = "d4hines";
  };
  networking.computerName = "YACHAL";
  networking.hostName = "YACHAL";
  users.users.d4hines = {
    name = "d4hines";
    home = "/Users/d4hines";
  };
  ids.gids.nixbld = 30000;
}
