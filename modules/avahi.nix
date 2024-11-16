{ ... }: {
  services.avahi.enable = true;
  services.avahi.nssmdns4 = true;
  # TODO: this doesn't seem to be working
  services.avahi.publish = {
    enable = true;
    domain = true;
    addresses = true;
  };
}
