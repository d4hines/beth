let
  big_yubikey = "age1yubikey1qwtacjfx8lra3q2ltg6svkj6v738hnl2tpx6ehxex3st46pf33fjgm79v84";
  #tiny_yubikey = "sk-ssh-ed25519@openssh.com AAAAGnNrLXNzaC1lZDI1NTE5QG9wZW5zc2guY29tAAAAIC6bNKOVnBNbCQXhPSjjy0//jlEq+qoElkA1GVI2362AAAAABHNzaDo= d4hines@YACHAL.local";
  ore_tiny_yubikey = "age1yubikey1qfwm96dqhv22v6cq9ja5te6ptnwqx4t37dqwkfu5qy38ade708n2y87lp97";
  user_keys = [
    big_yubikey
    #tiny_yubikey
    ore_tiny_yubikey
  ];
  ezra = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIC6UY8srtVvUY9hn3mX7GvLOCj+o74c2uaZJywvBupVF";
in
{
  "ezra-token.age".publicKeys = [ ezra ] ++ user_keys;
  "eds-survey-api-token.age".publicKeys = [ ezra ] ++ user_keys;
  "roam-token.age".publicKeys = [ ezra ] ++ user_keys;
  "rote-server-token.age".publicKeys = [ ezra ] ++ user_keys;
}
