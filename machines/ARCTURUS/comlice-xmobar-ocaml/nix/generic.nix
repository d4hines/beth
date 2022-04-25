{ pkgs, stdenv, lib, ocamlPackages, static ? false, doCheck }:
with ocamlPackages;
let preface = buildDunePackage {
  pname = "preface";
  version = "0.1.0";
  src = pkgs.fetchFromGitHub {
    owner = "xvw";
    repo = "preface";
    rev = "v0.1.0";
    sha256 = "yYzMhAhUAxy9BwZinVq4Zi1WzH0E8T9jHif9QQKcVLk=";
  };
  propagatedBuildInputs = [
    either
  ];
  doCheck = false;
};
in
rec {
  service = buildDunePackage {
    pname = "complice-xmobar";
    version = "0.1.0";

    src = lib.filterGitSource {
      src = ./..;
      dirs = [ "src" ];
      files = [ "dune-project" "complice-xmobar.opam" ];
    };

    # Static builds support, note that you need a static profile in your dune file
    buildPhase = ''
      echo "running ${if static then "static" else "release"} build"
      dune build bin/complice_xmobar.exe --display=short --profile=${if static then "static" else "release"}
    '';
    installPhase = ''
      mkdir -p $out/bin
      mv _build/default/bin/complice_xmobar.exe $out/bin/complice-xmobar
    '';

    checkInputs = [
    ];

    propagatedBuildInputs = [
      preface
      piaf-dream-compat
      yojson
      dream
      alcotest
      ppx_deriving
      ppx_deriving_yojson
    ];

    inherit doCheck;

    meta = {
      description = "Complice-xmobar integration server";
    };
  };
}
