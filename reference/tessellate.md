# Tessellate a [`dbSpatial`](https://dbverse-org.github.io/dbspatial-r/reference/dbSpatial.html) object

Creates a tessellation on the extent of a
[`dbSpatial`](https://dbverse-org.github.io/dbspatial-r/reference/dbSpatial.html)
object with specified parameters. Other input types are forwarded to
[`GiottoClass::tessellate()`](https://giotto-suite.github.io/GiottoClass/reference/tessellate.html).

## Usage

``` r
tessellate(x, extent, ...)

# S4 method for class 'dbSpatial,missing'
tessellate(
  x,
  extent,
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

- x:

  [`dbSpatial`](https://dbverse-org.github.io/dbspatial-r/reference/dbSpatial.html)
  object, or a positional extent-like object supported by
  [`GiottoClass::tessellate()`](https://giotto-suite.github.io/GiottoClass/reference/tessellate.html).

- extent:

  Optional named extent-like object supported by
  [`GiottoClass::tessellate()`](https://giotto-suite.github.io/GiottoClass/reference/tessellate.html).

- ...:

  Additional arguments passed to methods.

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

## Value

[`dbSpatial`](https://dbverse-org.github.io/dbspatial-r/reference/dbSpatial.html)
object for dbSpatial input, otherwise the result of
[`GiottoClass::tessellate()`](https://giotto-suite.github.io/GiottoClass/reference/tessellate.html).

## Details

`tessellate(x = dbSpatial, ...)` creates a tessellation over the
bounding box of the
[`dbSpatial`](https://dbverse-org.github.io/dbspatial-r/reference/dbSpatial.html)
geometry column and stores the result as a
[`dbSpatial`](https://dbverse-org.github.io/dbspatial-r/reference/dbSpatial.html)
table. `tessellate(extent = extent, ...)` and `tessellate(extent, ...)`
preserve the
[`GiottoClass::tessellate()`](https://giotto-suite.github.io/GiottoClass/reference/tessellate.html)
extent-based behavior. Supply either `x` or `extent`, but not both.

## Functions

- `tessellate(x = dbSpatial, extent = missing)`: Method for `dbSpatial`
  objects

## Examples

``` r
coordinates <- data.frame(x = c(100, 200, 300), y = c(500, 600, 700))
attributes <- data.frame(id = 1:3, name = c("A", "B", "C"))

# Combine the coordinates and attributes
dummy_data <- cbind(coordinates, attributes)

# Create a duckdb connection
con = DBI::dbConnect(duckdb::duckdb(), ":memory:")

# Create a duckdb table with spatial points
db_points = dbSpatial::dbSpatial(conn = con,
                                 value = dummy_data,
                                 x_colName = "x",
                                 y_colName = "y",
                                 name = "foo",
                                 overwrite = TRUE)

tessellate(db_points, name = "my_tessellation", shape = "hexagon", shape_size = 60)
#> 7 polygons generated
#> # Class:    dbSpatial 
#> # A query:  ?? x 2
#> # Database: DuckDB 1.5.4 [unknown@Linux 6.17.0-1018-azure:R 4.6.1/:memory:]
#>   poly_ID geom                             
#>   <chr>   <chr>                            
#> 1 ID_1    POLYGON ((160 564.641, 190 547...
#> 2 ID_2    POLYGON ((220 564.641, 250 547...
#> 3 ID_3    POLYGON ((130 616.6025, 160 59...
#> 4 ID_4    POLYGON ((190 616.6025, 220 59...
#> 5 ID_5    POLYGON ((250 616.6025, 280 59...
#> 6 ID_6    POLYGON ((160 668.5641, 190 65...
#> 7 ID_7    POLYGON ((220 668.5641, 250 65...
```
