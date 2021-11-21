with import <nixpkgs> {};

mkShell {
  buildInputs = [
    (sbclWithPackages (ps: with ps; [
      cl-opengl
      cl-liballegro
    ]))
  ];
}
