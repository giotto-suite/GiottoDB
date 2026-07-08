# Changelog

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
