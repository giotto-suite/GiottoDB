# Tessellate a [`dbSpatial`](https://dbverse-org.github.io/dbspatial-r/reference/dbSpatial.html) object

Creates a tessellation on the extent of
[`dbSpatial`](https://dbverse-org.github.io/dbspatial-r/reference/dbSpatial.html)
with specified parameters.

## Usage

``` r
tessellate(
  dbSpatial,
  geomName = "geom",
  name = "tessellation",
  shape = c("hexagon", "square"),
  shape_size = NULL,
  gap = 0,
  radius = NULL,
  overwrite = FALSE,
  ...
)

# S4 method for class 'dbSpatial'
tessellate(
  dbSpatial,
  geomName = "geom",
  name = "tessellation",
  shape = c("hexagon", "square"),
  shape_size = NULL,
  gap = 0,
  radius = NULL,
  overwrite = FALSE,
  ...
)
```

## Arguments

- geomName:

  `character string`. The geometry column name in the
  [`dbSpatial`](https://dbverse-org.github.io/dbspatial-r/reference/dbSpatial.html)
  object. Default: `"geom"`.

- name:

  `character string` name of table to add to
  [`dbSpatial`](https://dbverse-org.github.io/dbspatial-r/reference/dbSpatial.html)
  object. Default: "tessellation".

- shape:

  `character string`. A character string indicating the shape of the
  tessellation. Options are "hexagon" or "square".

- shape_size:

  `numeric`. the size of the shape in the tessellation. If `NULL`, a
  default size is calculated. See
  [`GiottoClass::tessellate`](https://giotto-suite.github.io/GiottoClass/reference/tessellate.html)
  for details.

- gap:

  `numeric`. Value indicating the gap between tessellation shapes.
  Defaults to 0.

- radius:

  `numeric`. Value specifying the radius for hexagonal tessellation.
  This parameter is ignored for square tessellations.

- overwrite:

  `logical`. Boolean value indicating whether to overwrite an existing
  tessellation with the same name. Default: `FALSE`.

- ...:

  Additional arguments passed to methods.

- [`dbSpatial`](https://dbverse-org.github.io/dbspatial-r/reference/dbSpatial.html):

  object

## Value

[`dbSpatial`](https://dbverse-org.github.io/dbspatial-r/reference/dbSpatial.html)
object

## Functions

- `tessellate(dbSpatial)`: Method for `dbSpatial` object

## Examples

``` r
coordinates <- data.frame(x = c(100, 200, 300), y = c(500, 600, 700))
attributes <- data.frame(id = 1:3, name = c("A", "B", "C"))

# Combine the coordinates and attributes
dummy_data <- cbind(coordinates, attributes)

# Create a duckdb connection
con = DBI::dbConnect(duckdb::duckdb(), ":memory:")

# Create a duckdb table with spatial points
db_points = dbSpatial(conn = con,
                      value = dummy_data,
                      x_colName = "x",
                      y_colName = "y",
                      name = "foo",
                      overwrite = TRUE)
#> Error in dbSpatial(conn = con, value = dummy_data, x_colName = "x", y_colName = "y",     name = "foo", overwrite = TRUE): could not find function "dbSpatial"

tessellate(db_points, name = "my_tessellation", shape = "hexagon", shape_size = 60)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'dbSpatial' in selecting a method for function 'tessellate': object 'db_points' not found
```
