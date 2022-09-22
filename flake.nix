{
  description = "A flake providing libfive, a framework for solid modeling using functional representations.";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    let

      sourceInfo =
        if self.sourceInfo ? rev
        then self.sourceInfo // {
          # Tag would have to be set manually for stable releases flake
          # because there's currently no way to get the tag via the interface.
          # tag = v1.0.0;
        }
        else (throw "Can't get revision because the git tree is dirty");

      version =
        if sourceInfo ? tag
        then sourceInfo.tag
        else with sourceInfo; builtins.substring 0 8 lastModifiedDate + "-" + shortRev;

      withPkgs = f: flake-utils.lib.eachDefaultSystem (system: f (import nixpkgs {
        inherit system;
      }));

    in

    withPkgs (pkgs:
      let
        libfive-studio = pkgs.libsForQt5.callPackage ./default.nix {
          inherit version;
        };
      in
      {

        packages.default = libfive-studio;
        apps.default = { type = "app"; program = "${libfive-studio}/bin/Studio"; };

      });
}
