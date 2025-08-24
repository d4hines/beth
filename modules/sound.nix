{
  pkgs,
  ...
}:
{
  environment.systemPackages = with pkgs; [ pulseaudio ];
  # Pipewire config
  services.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    # No idea if I need this
    alsa.support32Bit = true;
    pulse.enable = true;
  };
}
