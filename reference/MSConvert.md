# msConvert

msConvert

## Usage

``` r
msConvert2SciexMultipleWiff(
  raw.files,
  dir.to = dirname(raw.files),
  format.to = "mzML",
  BPPARAM = BiocParallel::SnowParam(workers = parallel::detectCores() - 1)
)
```

## Arguments

- raw.files:

  data input

- dir.to:

  dirname(raw.files)

- format.to:

  "mzML

- BPPARAM:

  BPPARAM

## Functions

- `msConvert2SciexMultipleWiff()`: Convert raw data from mass
  spectrometry
