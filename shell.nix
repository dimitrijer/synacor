let
  pkgs = import ./nix/default.nix { };
  sources = import ./nix/sources.nix;
  nix-filter = import sources.nix-filter;
  nixfiles = import sources.nixfiles { };
  neovim = nixfiles.neovim {
    pkgs = pkgs;
    withOCaml = true;
    withWriting = true;
  };
in
pkgs.mkShell {
  shellHooks = ''
    alias vim='nvim'
  '';

  buildInputs = with pkgs; [
    nixpkgs-fmt
    fswatch # for dune build -w
    ocamlformat
    ocamlPackages.odoc
    ocamlPackages.ocaml-lsp
    ocamlPackages.utop
    ocamlPackages.dune_3
    ocamlPackages.ounit2
    ocamlPackages.ocaml
    ocamlPackages.findlib
    ocamlPackages.janeStreet.base
    ocamlPackages.janeStreet.stdio
    ocamlPackages.janeStreet.ppx_jane
  ] ++ [ neovim ];
}
