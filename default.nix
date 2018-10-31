{ nixpkgs ? import ../../../nixpkgs {}
, compiler ? "ghc843"
}:

let

  inherit (nixpkgs) pkgs;
  inherit (pkgs) stdenv;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  pkg = haskellPackages.mkDerivation {
    pname = "htvm";
    version = "0.0.0";
    src = ./.;
    isLibrary = false;
    isExecutable = true;
    libraryHaskellDepends = with haskellPackages; [
      cabal-install ghc zlib haskdogs hasktags
      recursion-schemes
      Earley
      containers
      parsec
      pretty-show
      tasty
      tasty-hunit
      tasty-quickcheck
      HUnit
      unordered-containers
      deriving-compat
      haskdogs
    ];

    executableToolDepends = [ pkgs.clang_6 ];

    license = stdenv.lib.licenses.gpl3;

    shellHook = ''
      cabal() {( `which cabal` --ghc-options=-freverse-errors "$@" ; )}
    '';
  };

in
  if pkgs.lib.inNixShell then pkg.env else pkg
