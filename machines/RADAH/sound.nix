{ config, pkgs, options, ... }:

{
  sound.enable = true;
  sound.extraConfig = ''
  defaults.pcm.card 2
  defaults.ctl.card 2
  '';
  # Pipewire config
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    # No idea if I need this
    alsa.support32Bit = true;
    pulse.enable = true;
  };
}
