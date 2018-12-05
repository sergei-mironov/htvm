**WORK IN PROGRESS**

HTVM
====

This project contains an experimental Haskell frontend for [TVM](https://tvm.ai)
the Machine Learning framework. The goals of the frontend are:

 1. Provide EDSL wrappers for defining Machine Learning models in Haskell and
    compile them using TVM optimizing compiler.
 2. Provide C FFI able to run TVM models and marshal data to/from Haskell.

Design notes
------------

 1. EDSL is a proof-of-concept. It is implemented in `HTVM.EDSL` modules
    collection.
    1. `HTVM.EDSL.Types` module defines AST types which loosely corresponds to
       `Stmt` and `Expr` class hierarchies of TVM.
    2. `HTVM.EDSL.Monad` provides monadic interface to AST builders. We favored
       simplicity over type-safety. We belive that overuse of Haskell type
       system ruined many good libraries. The interface relies on simple ADTs
       whenever possible.
    3. Instead of building computational graph in memory, the interface
       translates AST to C++ and compiles it using `g++` and `clang` compilers.
       The translation is done by the `HTVM.EDSL.Printer` and `HTVM.EDSL.Build`
       modules. The data transformation pipeline goes as follows:

       ```

       Monadic    --> AST --> C++ --> Model --> LLVM --> Model
       Interface   .       .       .  Gen    .  asm   .  Library
                   .       .       .         .        .
                   .     Print     .       Print      .
                  Run             g++               clang

       ```
    4. We aim at supporting `import tvm` functionality. Adding support for
       [Relay](https://github.com/dmlc/tvm/issues/1673) is possible (e.g. by
       implementing Python printer).
    5. Support for Scheduling is minimal, but should be enhanced in future.
    6. Support for TOPI is minimal, but should be enhanced in future.
    7. No targets besides LLVM are supported. Adding them should be as hard as
       calling them from C++ DSL.
    8. We plan to support [Tensor-Level AD](https://sea-region.github.com/dmlc/tvm/issues/1996)

    Natural disadvantages of the current approach:
    - Compilation speed is limited by the speed of `g++`, which is quite slow.
    - Calling construction-time procedures of TVM is non-trivial.

    The pros are:
    - Implemented in <300 lines of code. Easy to maintain.
    - Easy to port to another TVM dialect such as Relay.
    - No need to debug TVM via Haskell. The problems (they are likely since TVM
      is in its betas!) may be handled with plain gdb suit.

 2. C FFI is implemented in `HTVM.Runtime.FFI` module. It does not depend on
    `HTVM.EDSL` and may be used to run models compiled by other TVM frontends.
    1. The module provide wrappers to basic `c_runtime_api.h` functions.
    2. `TVMArray` data is represented as ForeignPtrs to its Haskell
       representation.
    3. Currently, HTVM marshals from Haskell vectors and matrices, defined as
       plain lists. Support for `Data.Array` is planned.
    4. No backends besides LLVM are tested. Adding them should be quite simple.

Install
-------

We use development environment specified in [Nix](https://nixos.org/nix)
language. In order to use it, please install the
[Nix package manager](https://nixos.org/nix/download.html).
Having Nix manager and `NIX_PATH` set, enter the environment, by running Nix
development shell from the project's root folder:

    $ nix-shell

It should get all the dependencies upon the first run.  Alternatively, it should
be possible to use other Haskell distributions like
[Haskell Platform](https://www.haskell.org/platform/).

In the development shell it should be possible to use `cabal` to build the
project.

    $ cabal configure --enable-tests
    $ cabal build


One shoild be able to run tests with:

    $ cabal test

To enter the interactive shell, type

    $ cabal repl htvm

Usage examples may be found in [Tests](./test/Main.hs) and (possibly outdated)
[Demo](./src/Demo.hs).

Log
===

#### 06.12.2018
 * Support tuples and batchCompute
 * Improved this README

#### 29.11.2018
 * Add copyTensor FFI

#### 28.11.2018
 * Start to bind Schedulers
 * Improve monadic API
 * Free module and function after use in FFI

#### 24.11.2018
 * Add bindings for `conv2d` and `pad`
 * Compiler runners now may dump the program to file before running

#### 23.11.2018
 * Make `reduce_axis` example work
 * Comopile to `C++` is too slow, maybe generate `C++` was not a best idea. At
   least, it seems to be easer for printing than Python.
 * I feel that TenExpr/Expr separation is a bit artificial. One really needs a
   full-scale typechecker here.

#### 21.11.2018
 * Re-implement callTensorFunction
 * Module now contains information about its functions
 * FFI function call works!

#### 15.11.2018
 * Update callTensorFunction, still work in progress. DLTensor -> DLValue
   converter is needed
 * Complete the code of callTensorFunction, untested
 * ~~TODO: implement TVMTensor as a ForeignPtr~~
 * ~~TODO: write tests~~

#### 12.11.2018
 * [c2hs guide](https://github.com/haskell/c2hs/wiki/Implementation-of-Haskell-Binding-Modules)
 * Wrap module and function loaders (not working yet)

#### 10.11.2018
 * Improved FFI. Now it should be possible to allocate tensors from Haskell
 * Added stub for tests
 * Learned about `-XTypeApplications` option

#### 06.11.2018
 * Write FFI stub

#### 05.11.2018
 * Complete the bare-minimum EDSL, implement simple compile routine

#### 25.10.2018
 * Fix assign error. Drop show instance for StmtCtx, unfortunately.

#### 24.10.2018
 * Printer compiles

#### 22.10.2018
 * Started to implement Printer

#### 17.10.2018
 * Demo exposes DSL which compiles somehow

#### 15.10.2018
 * Write this README.md
