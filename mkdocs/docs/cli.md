# CLI

The `./CLI` folder contains various CLI tools for testing
and benchmarking.  All use Albert Dahlin's 
[elm/posix](https://package.elm-lang.org/packages/albertdahlin/elm-posix/latest/)
package.  Here are the entries for `.CLI/scripts.yaml`:

- lxparse: elm-cli run --debug src/LXPB.elm

  ```
  Example: vr lxparse lxtest/a1.txt
  Test the MicroLaTeX block parser
  ```

- l0parse: elm-cli run --debug src/L0PB.elm

  ```
   Example: vr l0parse l0test/datatable.txt
  Test the L0 block parser
   ```

- rt: elm-cli run src/RoundTrip.elm

  ```
  Example: vr rt rt/para.txt
  ```


- bench: time elm-cli run src/Benchmark.elm

  ```
  Example: vr bench init 100 bench/harmonic.tex
  ```