{
  pkgs ? import <nixpkgs> { },
}:

pkgs.writeShellApplication {
  name = "__SCRIPT_NAME__";

  runtimeInputs = with pkgs; [ __PACKAGES__ ];

  text = ''
    BASH_ARGV0="__SCRIPT_NAME__"
    # shellcheck source=/dev/null
    source "${__SCRIPT_SOURCE__}" "$@"
  '';
}
