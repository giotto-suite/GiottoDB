# GiottoDB Examples

This directory contains local example scripts for testing and development purposes.

**Note:** This folder is gitignored. Finalized, polished examples should be moved to the `vignettes/` directory.

## Available Examples

### 01_basic_usage.R
Comprehensive demonstration of GiottoDB spatPlot2D functionality including:
- Creating GiottoDB objects from Giotto objects
- Calling visualization backends directly (`GiottoDB:::.spatPlot2D_mosaic()`, `GiottoDB:::.spatPlot2D_deckgl()`) and using `spatPlot2D()` for regular giotto objects
- Discrete and continuous color mapping
- Cell selection and filtering
- Custom color palettes
- Point size and transparency options
- Persistent database storage
- Method comparisons

**Usage:**
```r
source("examples/01_basic_usage.R")
```

### 02_sqlrooms_demo.R
Integration with DBVisuals SQL Rooms for interactive database exploration:
- Creating persistent GiottoDB database
- Testing all visualization methods
- Launching SQL Rooms interface
- Proper cleanup procedures

**Usage:**
```r
source("examples/02_sqlrooms_demo.R")
```

**Requirements:** DBVisuals package must be installed

## Notes for Development

- These scripts use `GiottoData::loadGiottoMini()` for quick testing with small datasets
- Examples create temporary or in-memory databases by default
- SQL Rooms demo requires DBVisuals package (not on CRAN)
- Some interactive features (readline prompts) are commented out in the scripts

## Moving Examples to Vignettes

When an example is ready for public consumption:

1. Convert the R script to an Rmd vignette
2. Add proper vignette metadata
3. Include context and explanations
4. Test that the vignette builds successfully
5. Move to `vignettes/` directory
6. Update package documentation

Example vignette header:
```r
---
title: "Basic GiottoDB Usage"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Basic GiottoDB Usage}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---
```
