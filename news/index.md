# Changelog

## GiottoDB 0.99.0

### Bug fixes

- Improve reconnection after loading or moving GiottoDB projects,
  including top-level database connections, cached connections, and
  database lock-file handling.

- Fix Giotto-backed PCA and HVF edge cases, including argument
  forwarding in
  [`runPCA()`](https://giotto-suite.github.io/GiottoDB/reference/runPCA.md),
  k = 1 PCA scaling, and unsupported scaled-matrix selection.

- Improve in-memory coercion through
  [`as_giotto()`](https://giotto-suite.github.io/GiottoDB/reference/as_giotto.md)
  and safer placeholder handling in
  [`saveGiotto()`](https://giotto-suite.github.io/GiottoDB/reference/saveGiotto.md).

- Fix dbSpatial overlap outputs, including row and column names, scalar
  matrix outputs, geometry validation, and dynamic argument filtering in
  [`calculateOverlap()`](https://giotto-suite.github.io/GiottoDB/reference/calculateOverlap.md).

- Resolve package installation errors caused by regex escaping.

### Features

- Public release of package!

- Add dbSpatial-backed methods for spatial IDs, feature IDs, and
  subsetting.

- Add
  [`findMarkers_one_vs_all()`](https://giotto-suite.github.io/GiottoDB/reference/findMarkers_one_vs_all.md)
  support for GiottoDB objects.

- Support weighted
  [`overlapToMatrix()`](https://giotto-suite.github.io/GiottoDB/reference/overlapToMatrix.md)
  outputs for dbSpatial objects.

- Support conversion of `giottoPoints` spatial vectors to dbSpatial
  objects in
  [`as_giottodb()`](https://giotto-suite.github.io/GiottoDB/reference/as_giottodb.md).

### Chore

- Remove stale generated vignette sources.

- Update pkgdown configuration and site URL.

- Update package collate metadata and imports.

### Documentation

- Refresh pkgdown vignettes, benchmark figures, README, and package
  capability overview.

- Add concept tags and improve reference organization.

- Clarify scale suppression and unsupported normalization methods.

- Document
  [`as_giotto()`](https://giotto-suite.github.io/GiottoDB/reference/as_giotto.md)
  and clean up roxygen links.

### Code style

- Reformat package code with jarl and air.

### Refactoring

- Simplify geometry type retrieval in createGiottoPoints and
  createGiottoPolygon.

### Performance

- Materialize overlap views in
  [`overlapToMatrix()`](https://giotto-suite.github.io/GiottoDB/reference/overlapToMatrix.md)
  to improve performance and output handling.

### Testing

- Validate
  [`calculateHVF()`](https://giotto-suite.github.io/GiottoDB/reference/calculateHVF.md)
  residual variance behavior against upstream Giotto changes.

- Add reconnection, conversion, overlap, PCA, HVF, and full-pipeline
  test coverage.

- Update
  [`calculateOverlap()`](https://giotto-suite.github.io/GiottoDB/reference/calculateOverlap.md)
  test expectations for current behavior.

## GiottoDB 0.0.0.9001

### Bug fixes

- Remove missing GiottoData dep from vignettes.

- Replace dbMatrix::to_view with dbProject::to_view and update
  reconnection test expectations.

- OverlapToMatrix conversion to matrix.

- Missing DuckDB expression and simplify test.

### Features

- Add pkgdown workflow for building and deploying documentation site.

- `addStatistics` generic and S3 method for giottodb, giotto objects +
  add tests.

- `processData` methods.

### Chore

- Add ‘site’ to .Rbuildignore and .gitignore.

- Update gitignore to exclude VSCode workspace file.

- Update imports.

- Update .gitignore and .Rbuildignore.

- Require duckdb \>= 1.4.0.

- Migrate to dbverse-org.

- Add `dbProject` to imports.

- Update tests.

- Update pkg improts.

- Update vignette with fn table and GiottoDB functionality section.

- Add metadata.

### Documentation

- Update Rd files.

### Code style

- Apply air formatting to giottodb.

### Refactoring

- .onLoad in zzz.R.

### Testing

- Fix warning.

### Uncategorized

- Merge branch ‘svd’.
