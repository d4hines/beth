{ pkgs, ... }:
{
  imports = [ ../../modules/darwin ];
  environment.variables = {
    DEFAULT_USER = "dhines";
  };
  networking.computerName = "MALAK";
  networking.hostName = "MALAK";
  users.users.dhines = {
    name = "dhines";
    home = "/Users/dhines";
  };
  environment.systemPackages = with pkgs; [ tinyproxy ];
  environment.etc."tinyproxy/tinyproxy.conf".text = ''
    Port 12345
    Listen 127.0.0.1
    Timeout 600
    Allow 127.0.0.1
    LogLevel Connect

    # Block all the shit
    FilterType ere
    FilterURLs On
    FilterDefaultDeny Yes
    Filter "/etc/tinyproxy/whitelist"
  '';
  launchd.user.agents."tinyproxy" = {
    command = "${pkgs.tinyproxy}/bin/tinyproxy -c /etc/tinyproxy/tinyproxy.conf -d";
    serviceConfig = {
      KeepAlive = true;
      RunAtLoad = true;
      StandardOutPath = "/tmp/tinyproxy.log";
      StandardErrorPath = "/tmp/tinyproxy.error.log";
    };
  };
}
