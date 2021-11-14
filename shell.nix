with import <nixpkgs> {};

mkShell {
  buildInputs = [
    (sbclWithPackages (ps: with ps; [
      cl-opengl
      cl-glu
      cl-glut
      cl-liballegro
    ]))
  ];
}
