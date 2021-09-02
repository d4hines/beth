final: prev: with prev; {
  my_logseq = stdenv.mkDerivation rec {
    pname = "logseq";
    version = "0.3.7";

    src = fetchurl {
      url = "https://github.com/logseq/logseq/releases/download/${version}/logseq-linux-x64-${version}.AppImage";
      sha256 = "awkJbD0NTc50vn/u5IZ2KH0VDOSEjRFjREmU0PBb4Ag=";
      name = "${pname}-${version}.AppImage";
    };

    appimageContents = appimageTools.extract {
      name = "${pname}-${version}";
      inherit src;
    };

    dontUnpack = true;
    dontConfigure = true;
    dontBuild = true;

    nativeBuildInputs = [ makeWrapper ];

    installPhase = ''
      runHook preInstall

      mkdir -p $out/bin $out/share/${pname} $out/share/applications
      cp -a ${appimageContents}/{locales,resources} $out/share/${pname}
      cp -a ${appimageContents}/Logseq.desktop $out/share/applications/${pname}.desktop

      substituteInPlace $out/share/applications/${pname}.desktop \
        --replace Exec=Logseq Exec=${pname} \
        --replace Icon=Logseq Icon=$out/share/${pname}/resources/app/icons/logseq.png

      runHook postInstall
    '';

    postFixup = ''
      makeWrapper ${electron}/bin/electron $out/bin/${pname} \
        --add-flags $out/share/${pname}/resources/app
      dugite=$out/share/${pname}/resources/app/node_modules/dugite
      rm -f $dugite/git/bin/git
      ln -s ${git}/bin/git $dugite/git/bin/git
      rm -f $dugite/git/libexec/git-core/git
      ln -s ${git}/bin/git $dugite/git/libexec/git-core/git  
    '';

    passthru.updateScript = ./update.sh;

    meta = with lib; {
      description = "A local-first, non-linear, outliner notebook for organizing and sharing your personal knowledge base";
      homepage = "https://github.com/logseq/logseq";
      license = licenses.agpl3Plus;
      maintainers = with maintainers; [ weihua ];
      platforms = [ "x86_64-linux" ];
    };
  };
}
