# Create giotto polygon object with dbSpatial

Create giotto polygon object with dbSpatial

## Usage

``` r
create_giotto_polygon_object_db(
  name = "polygons",
  dbSpatial = NULL,
  unique_IDs = NULL,
  calc_centroids = FALSE
)
```

## Arguments

- name:

  name for the polygon object

- dbSpatial:

  dbSpatial object containing polygon data

- unique_IDs:

  (optional) unique IDs for cacheing

- calc_centroids:

  logical. Whether to calculate centroids

## Value

giotto_polygon_object
