# Plot in situ points with GiottoDB support

S3 wrapper for
[`GiottoVisuals::spatInSituPlotPoints()`](https://giotto-suite.github.io/GiottoVisuals/reference/spatInSituPlotPoints.html)
that converts dbSpatial-backed spatial slots to in-memory spatial
objects before plotting.

## Usage

``` r
spatInSituPlotPoints(gobject, ...)

# S3 method for class 'giotto'
spatInSituPlotPoints(gobject, ...)

# S3 method for class 'GiottoDB'
spatInSituPlotPoints(gobject, ...)
```

## Arguments

- gobject:

  A giotto or GiottoDB object.

- ...:

  Additional arguments passed to
  [`GiottoVisuals::spatInSituPlotPoints()`](https://giotto-suite.github.io/GiottoVisuals/reference/spatInSituPlotPoints.html).

## Value

A ggplot object or plot object returned by
[`GiottoVisuals::spatInSituPlotPoints()`](https://giotto-suite.github.io/GiottoVisuals/reference/spatInSituPlotPoints.html).
