final: prev:
with prev; let
  gcloud-auth-plugin-deb = stdenv.mkDerivation {
    name = "google-cloud-sdk-gke-gcloud-auth-plugin-deb";
    src = prev.fetchurl {
      url = "https://packages.cloud.google.com/apt/pool/google-cloud-cli-gke-gcloud-auth-plugin_393.0.0-0_amd64_04cca5f6ba7035954886226c519fa3627f97750bb664371b3def0864d6889e92.deb";
      sha256 = "14lyi3b6827g7ldkfr5n1dsrfzv2lfgm2v12hr49adbhpbvabk04";
    };
    nativeBuildInputs = [
      dpkg
      makeWrapper
    ];
    unpackPhase = "true";
    installPhase = ''
      mkdir -p $out/bin
      dpkg -x $src $out
      ln -s $out/usr/lib/google-cloud-sdk/bin/gke-gcloud-auth-plugin $out/bin
    '';
  };
in {
  gke-gcloud-auth-plugin = buildFHSUserEnv {
    name = "gke-gcloud-auth-plugin";
    targetPkgs = pkgs: [gcloud-auth-plugin-deb];
    multiPkgs = pkgs: [pkgs.dpkg];
    runScript = "gke-gcloud-auth-plugin";
  };
}
