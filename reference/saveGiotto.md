# Save Giotto object

Generic function to save Giotto or GiottoDB objects

## Usage

``` r
saveGiotto(gobject, ...)

# S3 method for class 'GiottoDB'
saveGiotto(
  gobject,
  foldername = "saveGiottoDir",
  dir = NULL,
  method = c("RDS", "qs"),
  method_params = list(),
  overwrite = FALSE,
  export_image = TRUE,
  image_filetype = "PNG",
  include_feat_coord = TRUE,
  verbose = TRUE,
  ...
)

# Default S3 method
saveGiotto(
  gobject,
  foldername = "saveGiottoDir",
  dir = NULL,
  method = c("RDS", "qs"),
  method_params = list(),
  overwrite = FALSE,
  export_image = TRUE,
  image_filetype = "PNG",
  include_feat_coord = TRUE,
  verbose = TRUE,
  ...
)
```

## Arguments

- gobject:

  giotto or GiottoDB object

- ...:

  additional parameters passed to methods

- foldername:

  Folder name

- dir:

  Directory where to create the folder

- method:

  method to save main object

- method_params:

  additional method parameters for RDS or qs

- overwrite:

  Overwrite existing folders

- export_image:

  logical. Write out an image when saving giottoLargeImage

- image_filetype:

  the image filetype to use. Default is "PNG"

- include_feat_coord:

  logical. Whether to keep feature coordinates

- verbose:

  be verbose

## Value

Creates a directory with GiottoDB object information including database
files

## Details

This method extends GiottoClass::saveGiotto to handle database-backed
objects. The database file (.db) is moved to the save directory, and
connection information is updated to point to the new location. Note:
The original database file will no longer exist at its original location
after saving.

## Methods (by class)

- `saveGiotto(GiottoDB)`: Save a GiottoDB object

- `saveGiotto(default)`: Default method - delegates to GiottoClass
