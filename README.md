HTVM
====

HTVM is a library which provides Haskell runtime and experimental frontend for
[TVM](https://tvm.ai) the Machine Learning framework.

**The project is under development and is not ready for use**


TVM in a nutshell
-----------------

[TVM](https://tvm.ai) framework extends Halide [(link)](https://halide.io)
principles to the Machine earning domain. It offeres (a) EDSLs for defining ML
models (b) import facilities for translating models from other frameworks
such as TensorFlow and (c) compiler to binary code for a variety of supported
platforms, including LLVM (x86, arm), CUDA, OpenCL, Vulcan, ROCm, FPGAs and even
WebAssembly (note: level of support may vary).  DSLs for C++ and Python are best
supported and also there are some support for Java, Go and Rust languages.

Originally, TVM aimed at increasing speed of model's inference by providing a
rich set of optimizing primitives (called 'schedules'). At the same time it had
little support for training models. Recently, proposals of adding training
functionality were [added](https://sea-region.github.com/dmlc/tvm/issues/1996).

TVM aims at compiling ML models in highly optimized binary code.

Important parts of TVM are:
  * `tvm` is a core library providing `compute` interface.
  * `topi` is a tensor operations collection. Most of the middle-layer
    primitives such as `matmul`, `conv2d` and `softmax` are defined there.
  * `relay` is a high-level library written in Python, providing
    functional-style interface and its own typechecker. Currently, relay is
    under active development and beyond the scope of HTVM.
  * `nnvm` is another high-level wrapper in Python, which is deprecated.

HTVM goals
----------

This project provides Haskell bindings for:

 1. TVM C Runtime, which makes it possible to run ML models from
    Haskell programs
 2. Proof-of-concept EDSL for defining Machine Learning models in Haskell

Usage
-----

TVM, gcc, llvm should be installed.
TODO: Briefly describe how to install TVM (which is not hard), provide Demo code.

Design notes
------------

### TVM C Runtime

FFI for TVM C Runtime library is a Haskell package, linked to
`libtvm_runtime.so`. This library contains functionality, required to load and
run ML code produced by TVM.

 1. The module provide wrappers to `c_runtime_api.h` functions.
 2. `TVMArray` is the main type describing Tensors in TVM. It is represented as
    ForeignPtr to internal representation and a set of accessor functions.
 3. Currently, HTVM can marshal data from Haskell lists. Support for
    `Data.Array` is planned.
 4. No backends besides LLVM are tested. Adding them should not be hard and is
    on the TODO list.

### TVM Haskell EDSL

EDSL has a proof-of-concept status. It may be used to declare ML models in
Haskell, convert them to TVM IR and finally compile.  Later, compiled model may be
loaded and run with Haskell FFI or with any other runtime supported by TVM.

Contrary to usual practices, we don't manipulate TVM IR by calling TVM functions
internally. Instead, we build AST in Haskell and print it to C++ program. After
that we compile the program with common instruments. This approach has its pros and
cons, which are described below.

 1. `HTVM.EDSL.Types` module defines AST types which loosely corresponds to
    `Stmt` and `Expr` class hierarchies of TVM.
 2. `HTVM.EDSL.Monad` provides monadic interface to AST builders. We favored
    simplicity over type-safety. We belive that overuse of Haskell type system
    ruined many good libraries. The interface relies on simple ADTs whenever
    possible.
 3. `HTVM.EDSL.Print` contain funcions which print AST to C++ program of Model
    Generator.
 4. `HTVM.EDSL.Build` provides instruments to compile and run the model
    generator by executing `g++` and `clang` compilers:
    * The Model Generator program builds TVM IR and performes target code
      generation. In HTVM, we support LLVM target, but more targets may be added.
    * We execute `clang` to compile LLVM into x86 '.so' library. Resulting
      library may be loaded and executed by the Runtime code.

The whole data transformation pipeline goes as follows:

```

Monadic    --> AST --> C++ --> Model --> LLVM --> Model --> Runtime FFI
Interface   .       .       .  Gen    .  asm   .  Library
            .       .       .         .        .
            .     Print     .       Print      .
           Run    C++      g++               clang

```

Known disadvantages of C++ printing approach are:
- **Compilation speed is limited by the speed of `g++`, which is slow.** Gcc is
  used to compile C++ to binary which may take as long as 5 seconds. Little may
  be done about that without changing approaches. One possible way to overcome
  this limitation would be to provide direct FFI to TVM IR like
  [Halide-hs](https://github.com/cchalmers/halide-hs) does for Halide.
  Unfortunately, this approach has its own downsides:
  * Low-level IR API is not as stable as its high-level counterpart
  * TVM is in its early stages and sometimes crashes. FFI to IR would provide no
    isolation from this.
- **Calling construction-time procedures of TVM is non-trivial.** This is a
  consequence of previous limitation. For example, TVM may calculate Tensor
  shape in runtime and use it immediately to define new Tensors. In order to
  that in Haskell we would need to compile and run C++ program which is possible
  by slow. We try to avoid calling construction-time procedures.
- **User may face weird C++ errors**. TVM is quite a low-level library which
  offers little type-checking, so user may write bad programs easily. Other high
  level TVM wrappers like Relay in Python, does provide their own typecheckers
  to catch errors earlier. HTVM offers no typechecker currently but it is
  certainly possible to write one. Contributions are welcome!

The pros of this approach are:
- C++ printer is implemented in less than 300 lines of code. Easy to maintain.
- Easy to port to another TVM dialect such as Relay.
- Isolation from TVM crashes. Memory problems of TVM IR will be translated to error
  messages in Haskell.

Future plans
------------

 * We aim at supporting basic `import tvm` and `import topi` functionality.
 * Support for Scheduling is minimal, but should be enhanced in future.
 * Support for TOPI is minimal, but should be enhanced in future.
 * No targets besides LLVM are supported. Adding them should be as simple as
   calling them from C++ DSL.
 * We plan to support [Tensor-Level AD](https://sea-region.github.com/dmlc/tvm/issues/1996)
 * Adding support for [Relay](https://github.com/dmlc/tvm/issues/1673) is also
   possible but may require some efforts like writing Python printer.

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
