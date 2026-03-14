# linear-algebra

OCaml library for matrix operations and linear algebra.

## Build

```bash
dune build
```

## Run tests

```bash
dune test
```

## Check code coverage

Run tests with coverage instrumentation:

```bash
dune runtest --instrument-with bisect_ppx
```

Generate HTML coverage report:

```bash
bisect-ppx-report html
```

The report will be available at `_coverage/index.html`

## License

This project is licensed under the terms of the [LICENSE](LICENSE) file.
