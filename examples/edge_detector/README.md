# Edge detector

This example showcases the use of *optional outputs*. A boolean input signal is analysed by the **edge_detect** node, which outputs `true` whenever the signal changes from `false` to `true`, `false` when the signal switches from `true` to `false`, and nothing otherwise.

## Testing harness

This example is set up as an automated test. The input signal is defined at the top of `edge_detector_test.ml` as a list of boolean values. After the values of this list have been used, the signal stays `false`.

## Running the test

This example is automatically run when `dune runtest` is called in the toplevel directory of this repository. It can also be run individually:

```bash
dune exec -- edge_detector_test.exe
```

If there is no output, all edges were successfully detected.
