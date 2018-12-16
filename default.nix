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
      temporary
      directory
      mnist-idx
    ];

    executableToolDepends = [ pkgs.clang_6 haskellPackages.c2hs ];

    license = stdenv.lib.licenses.gpl3;

    shellHook = ''
      cabal() {( `which cabal` --ghc-options=-freverse-errors "$@" ; )}

      # Fix g++(v7.3): error: unrecognized command line option ‘-stdlib=libstdc++’; did you mean ‘-static-libstdc++’?
      unset NIX_CXXSTDLIB_LINK NIX_TARGET_CXXSTDLIB_LINK

      export CWD=`(cd ../../..; pwd)`

      export TVM=$CWD/src/$USER/tvm
      export BUILD=build-docker
      export PYTHONPATH="$CWD/src/$USER:$TVM/python:$TVM/topi/python:$TVM/nnvm/python:$PYTHONPATH"
      export LD_LIBRARY_PATH="$TVM/$BUILD:$LD_LIBRARY_PATH"
      export C_INCLUDE_PATH="$TVM/include:$TVM/3rdparty/dmlc-core/include:$TVM/3rdparty/HalideIR/src:$TVM/3rdparty/dlpack/include:$TVM/topi/include:$TVM/nnvm/include"
      export CPLUS_INCLUDE_PATH="$C_INCLUDE_PATH"
      export LIBRARY_PATH=$TVM/$BUILD
    '';
  };

in
  if pkgs.lib.inNixShell then pkg.env else pkg
