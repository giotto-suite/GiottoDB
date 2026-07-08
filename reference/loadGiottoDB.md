# Load GiottoDB object with explicit connection

Helper function to load a Giotto object and convert it to GiottoDB using
an existing connection.

## Usage

``` r
loadGiottoDB(path_to_folder, con, ...)
```

## Arguments

- path_to_folder:

  path to folder where object was stored

- con:

  DBI connection to use

- ...:

  additional parameters passed to loadGiotto
