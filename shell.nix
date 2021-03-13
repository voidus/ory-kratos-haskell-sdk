{ system ? builtins.currentSystem }:
let
  pkgs = import ./nix { inherit system; };
in
pkgs.mkShell {
  buildInputs = [
    pkgs.ory-kratos.shell
  ];
  shellHook = ''
    export LD_LIBRARY_PATH=${pkgs.ory-kratos.shell}/lib:$LD_LIBRARY_PATH
    logo
  '';
}
