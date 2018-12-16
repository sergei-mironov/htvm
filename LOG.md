Log
===

#### 16.12.2018
 * Add ForeingPtr-based Module and function API
 * Provide type and shape checks for tensor FFI
 * Started to write demo, learn how to load MNIST

#### 10.12.2018
 * Bind Differentiate primitive, test works

#### 09.12.2018
 * Improved README.md
 * Started autodiff integration

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

