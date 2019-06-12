{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let
  tex-environment = texlive.combine {
    inherit (texlive)
      scheme-basic
      # unitn stuff
      setspace
      titlesec
      epstopdf
      # my additions
      pgfplots
      pgf
      xcolor
    ;
  };
  filename = "Gazzetta_Francesco_Informatica_19";

in stdenv.mkDerivation {
  name = "thesis";
  src = ./.;
  nativeBuildInputs = [ tex-environment ghostscript ];
  buildPhase = ''
    pdflatex ${filename}.tex
    bibtex ${filename}.aux
    pdflatex ${filename}.tex
    pdflatex ${filename}.tex
  '';
  installPhase = ''
    install -Dm644 \
      ${filename}.pdf \
      $out/${filename}.pdf
  '';
}

