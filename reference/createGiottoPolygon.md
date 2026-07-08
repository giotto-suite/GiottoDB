# Create GiottoPolygon object using dbSpatial

Create a `giottoPolygon` object that wraps a dbSpatial polygons object
to support larger-than-memory spatial polygon data. This implementation
extends the standard GiottoClass implementation by providing specific
methods for dbSpatial objects.

## Usage

``` r
# S4 method for class 'dbSpatial'
createGiottoPolygon(
  x,
  name = "polygons",
  verbose = TRUE,
  split_keyword = NULL,
  unique_IDs = NULL,
  calc_centroids = FALSE
)
```

## Arguments

- x:

  dbSpatial object, SpatVector, or data.frame-like object with polygon
  coordinate information (must include poly_ID column)

- name:

  character. Name for the polygon object

- verbose:

  be verbose

- split_keyword:

  list of character vectors of keywords to split the giottoPolygon
  object based on their poly_ID. Keywords will be
  [`grepl()`](https://rdrr.io/r/base/grep.html) matched against the
  polygon IDs information.

- unique_IDs:

  (optional) character vector of unique IDs present within the
  spatVector data. Provided for cacheing purposes

- calc_centroids:

  logical. Whether to calculate centroids for the polygons

## Value

giottoPolygon object wrapping a dbSpatial object
