with import <nixpkgs> {};

mkShell {
  buildInputs = [
    (sbclWithPackages (ps: with ps; [
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
