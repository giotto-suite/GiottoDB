# Create giotto points object with dbSpatial

Create giotto points object with dbSpatial

## Usage

``` r
create_giotto_points_object_db(
  feat_type = "rna",
  dbSpatial = NULL,
  networks = NULL,
  unique_IDs = NULL
)
```

## Arguments

- feat_type:

  feature type

- dbSpatial:

  dbSpatial object containing point data

- networks:

  (optional) feature network object

- unique_IDs:

  (optional) unique IDs for cacheing

## Value

giotto_points_object
