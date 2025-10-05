{ ... }:
{
  imports = [ ../../modules/darwin ];
  system.primaryUser = "dhines";
  environment.variables = {
    DEFAULT_USER = "dhines";
  };
  networking.computerName = "MALAK";
  networking.hostName = "MALAK";
  users.users.dhines = {
    name = "dhines";
    home = "/Users/dhines";
  };
}
