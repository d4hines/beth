{pkgs, ...}: {
  imports = [../../modules/darwin];
  environment.variables = {
    DEFAULT_USER = "d4hines";
  };
  networking.computerName = "YACHAL";
  networking.hostName = "YACHAL";
  users.users.d4hines = {
    name = "d4hines";
    home = "/Users/d4hines";
  };
}
