{ sources }:
[
  (final: prev: {
    inherit (import sources.gitignore { inherit (prev) lib; }) gitignoreFilter;
  })
  (final: prev: {
    ory-kratos = import ./packages.nix { pkgs = prev; };
  })
]
