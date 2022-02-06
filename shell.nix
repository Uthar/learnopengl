let
  pkgs = import (builtins.fetchTarball {
    url = "https://github.com/nixos/nixpkgs/archive/21.11.tar.gz";
    sha256 = "162dywda2dvfj1248afxc45kcrg83appjd0nmdb541hl7rnncf02";
  }) {};
  nix-cl = import (builtins.fetchTarball {
    url = "https://github.com/uthar/nix-cl/archive/c6c087de2802812bc9d5baa4c72955009050c1aa.tar.gz";
    sha256 = "19hyhg55jj3w6d71l9c5xp4gik5ykjqbdbzd0y1f5xhbv68ysb0b";
  }) { inherit pkgs; };
in

pkgs.mkShell {
  buildInputs = [
    (nix-cl.sbclWithPackages (ps: with ps; [
      alexandria
      cl-opengl
      cl-liballegro
      _3d-matrices
      _3d-vectors
      png-read
      cl-jpeg
      classimp
    ]))
  ];
}
