# Calculate Highly Variable Features for GiottoDB

S3 method that prevents accessing "scaled" expression to avoid
materialization.

## Usage

``` r
calculateHVF(gobject, ...)

# S3 method for class 'GiottoDB'
calculateHVF(
  gobject,
  spat_unit = NULL,
  feat_type = NULL,
  expression_values = "normalized",
  method = c("cov_groups", "cov_loess", "var_p_resid"),
  reverse_log_scale = FALSE,
  logbase = 2,
  expression_threshold = 0,
  nr_expression_groups = 20,
  zscore_threshold = 1.5,
  HVFname = "hvf",
  difference_in_cov = 0.1,
  var_threshold = 1.5,
  var_number = NULL,
  random_subset = NULL,
  set_seed = TRUE,
  seed_number = 1234,
  show_plot = NULL,
  return_plot = NULL,
  save_plot = NULL,
  save_param = list(),
  default_save_name = "HVFplot",
  return_gobject = TRUE,
  calc_gini = TRUE,
  verbose = TRUE
)

# S3 method for class 'giotto'
calculateHVF(
  gobject,
  spat_unit = NULL,
  feat_type = NULL,
  expression_values = c("normalized", "scaled", "custom"),
  method = c("cov_groups", "cov_loess", "var_p_resid"),
  reverse_log_scale = FALSE,
  logbase = 2,
  expression_threshold = 0,
  nr_expression_groups = 20,
  zscore_threshold = 1.5,
  HVFname = "hvf",
  difference_in_cov = 0.1,
  var_threshold = 1.5,
  var_number = NULL,
  random_subset = NULL,
  set_seed = TRUE,
  seed_number = 1234,
  show_plot = NULL,
  return_plot = NULL,
  save_plot = NULL,
  save_param = list(),
  default_save_name = "HVFplot",
  return_gobject = TRUE,
  calc_gini = TRUE,
  verbose = TRUE
)
```

## Arguments

- gobject:

  giotto object

- spat_unit:

  spatial unit

- feat_type:

  feature type

- expression_values:

  expression values to use

- method:

  method to calculate highly variable features

- reverse_log_scale:

  reverse log-scale of expression values (default = FALSE)

- logbase:

  if `reverse_log_scale` is TRUE, which log base was used?

- expression_threshold:

  expression threshold to consider a gene detected

- nr_expression_groups:

  (cov_groups) number of expression groups for cov_groups

- zscore_threshold:

  (cov_groups) zscore to select hvg for cov_groups

- HVFname:

  name for highly variable features in cell metadata

- difference_in_cov:

  (cov_loess) minimum difference in coefficient of variance required

- var_threshold:

  (var_p_resid) variance threshold for features for var_p_resid method

- var_number:

  (var_p_resid) number of top variance features for var_p_resid method

- random_subset:

  random subset to perform HVF detection on. Passing `NULL` runs HVF on
  all cells.

- set_seed:

  logical. whether to set a seed when random_subset is used

- seed_number:

  seed number to use when random_subset is used

- show_plot:

  show plot

- return_plot:

  return ggplot object (overridden by `return_gobject`)

- save_plot:

  logical. directly save the plot

- save_param:

  list of saving parameters from
  [`GiottoVisuals::all_plots_save_function()`](https://giotto-suite.github.io/GiottoVisuals/reference/plot_save.html)

- default_save_name:

  default save name for saving, don't change, change save_name in
  save_param

- return_gobject:

  boolean: return giotto object (default = TRUE)

- calc_gini:

  logical. Whether to calculate Gini index for each feature. Set to
  FALSE for performance with large datasets or dbMatrix objects.

- verbose:

  be verbose
