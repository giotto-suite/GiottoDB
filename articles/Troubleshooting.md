# Troubleshooting & FAQ

This vignette covers common issues you may encounter when using
`GiottoDB` and how to resolve them.

## Giotto functions do not work with GiottoDB objects

### Symptom

After loading `Giotto`, `GiottoClass`, `GiottoVisuals`, or the full
Giotto Suite after `GiottoDB`, calls such as `runPCA(gobject)`,
`normalizeGiotto(gobject)`, `calculateHVF(gobject)`, or
`saveGiotto(gobject)` may use the standard Giotto implementation instead
of the GiottoDB implementation.

### Cause

Some Giotto functions are exported as regular functions rather than
shared generics. In R, packages loaded later appear earlier on the
search path. Loading `Giotto` after `GiottoDB` can therefore mask
GiottoDB wrappers with functions of the same name.

``` r

library(GiottoDB)
library(Giotto)

# In this load order, runPCA() resolves to Giotto::runPCA().
find("runPCA")
```

This does not remove the GiottoDB implementation. It only changes which
function is found first when an unqualified function name is used.

### Solutions

Always load `GiottoDB` last when using unqualified function names:

``` r

library(Giotto)
library(GiottoDB)

gobject <- normalizeGiotto(gobject)
gobject <- calculateHVF(gobject)
gobject <- runPCA(gobject)
saveGiotto(gobject, foldername = "my_giottodb_object")
```

Alternatively, call GiottoDB functions explicitly:

``` r

gobject <- GiottoDB::normalizeGiotto(gobject)
gobject <- GiottoDB::calculateHVF(gobject)
gobject <- GiottoDB::runPCA(gobject)
GiottoDB::saveGiotto(gobject, foldername = "my_giottodb_object")
```

This is especially important for functions with GiottoDB-specific
database-backed behavior, including expression processing, PCA, saving
and loading, and selected visualization methods.

A longer-term solution is planned to define more shared generics
upstream in Giotto or GiottoClass, with Giotto and GiottoDB each
registering methods for their supported object classes.

## Why does this happen?

GiottoDB extends Giotto workflows by adding methods and wrappers for
database-backed objects when needed. For functions that are not shared
S3 or S4 generics in the Giotto API, R cannot dispatch to GiottoDB
methods if a later-loaded package masks the GiottoDB wrapper.

## How can I check which function is being called?

Use [`find()`](https://rdrr.io/r/utils/apropos.html) or inspect the
function environment:

``` r

find("runPCA")
environmentName(environment(runPCA))
environmentName(environment(GiottoDB::runPCA))
```

If the unqualified function resolves to `Giotto`, `GiottoClass`, or
`GiottoVisuals`, use `GiottoDB::function_name()` or reload `GiottoDB`
last.

## Getting help

If you encounter issues not covered here:

1.  Visit the [GiottoDB issue
    page](https://github.com/giotto-suite/giottodb/issues).
2.  Search for existing issues/errors that match your problem.
3.  If you cannot find a solution, open a new issue and include a
    minimal reproducible example.
