{
  pkgs ? import <nixpkgs> { },
}:

let
  pythonEnv = pkgs.python3.withPackages (ps: with ps; [ __PYTHON_PACKAGES__ ]);
  fn = "bin/__SCRIPT_NAME__";
  wrapper = ''
    #!${pythonEnv}/bin/python
    from sys import argv
    argv[0] = "__SCRIPT_NAME__"
  '';
in
pkgs.stdenv.mkDerivation {
  name = "__SCRIPT_NAME__";

  src = builtins.path { path = __SCRIPT_SOURCE__; };
  dontUnpack = true;

  buildPhase = ''
    mkdir bin

    cat <<EOF > "${fn}"
    ${wrapper}
    EOF

    cat "$src" >> "${fn}"
  '';

  installPhase = ''
    mkdir -p $out
    mv bin $out/
    chmod +x "$out/${fn}"
  '';
}
