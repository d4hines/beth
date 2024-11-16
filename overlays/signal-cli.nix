final: prev:
with prev; {
  signal-cli = prev.signal-cli.overrideAttrs (_:
    let
      version = "0.12.2";
    in
    {
      inherit version;
      src = fetchurl {
        url = "https://github.com/AsamK/signal-cli/releases/download/v${version}/signal-cli-${version}-Linux.tar.gz";
        hash = "sha256-XhLTovymqjbc19X717WyNIi4jdpwnyttXGqkkHBFwQA=";
      };
    });
}
