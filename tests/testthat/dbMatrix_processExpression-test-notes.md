# dbMatrix processExpression Test Notes

## Test Setup
First, load all local Giotto packages to ensure we're working with the latest development versions:

```r
devtools::load_all("/Users/user/Documents/dries/GiottoSuite/GiottoUtils")
devtools::load_all("/Users/user/Documents/dries/GiottoSuite/GiottoClass")
devtools::load_all("/Users/user/Documents/dries/GiottoSuite/GiottoVisuals")
devtools::load_all("/Users/user/Documents/dries/GiottoSuite/Giotto")
devtools::load_all("/Users/user/Documents/dries/GiottoSuite/GiottoDB")
```

## Test Plan
1. Run the entire `test-dbMatrix_processExpression.R` file
2. Document any failures with detailed information
3. Propose fixes but check before implementing them

## Test Execution

### Initial Run

Command used to run test:
```r
testthat::test_file("tests/testthat/test-dbMatrix_processExpression.R")
```

#### Observations:
- [ ] Check if dbMatrix correctly handles library normalization
- [ ] Check if dbMatrix correctly handles log normalization
- [ ] Verify Z-score scaling across features works as expected
- [ ] Verify Z-score scaling across cells works as expected
- [ ] Test whether the full pipeline with default processing params works correctly
- [ ] Verify that dbMatrix memory contract is respected

## Test Results - [2025-04-23 21:38]

### Run Summary
- **Failed Tests**: 2
- **Warnings**: 5
- **Skipped Tests**: 0
- **Passed Tests**: 0

### Issues and Resolutions

#### Issue 1: S4 object not subsettable
- **Test:** "processExpression works with dbMatrix input"
- **Error message:** "Error in `ex[][filter_bool_feats, filter_bool_cells, drop = FALSE]`: object of type 'S4' is not subsettable"
- **Root cause:** The error occurs when trying to subset a dbMatrix object during the `filterGiotto()` function call. The subsetting operation in `.subset_expression_data` doesn't correctly handle dbMatrix S4 objects.
- **Proposed fix:** Update the subsetting methods for dbMatrix objects in GiottoClass to properly handle S4 subsetting operations.

#### Issue 2: Expression object not recognized
- **Test:** "dbMatrix memory contract is respected in processExpression"
- **Error message:** "Error: [GiottoClass] .evaluate_expr_matrix(expression_data, expression_matrix_class = expression_matrix_class, : [GiottoClass] feat_type = feat_type): expression input needs to be a path to matrix-like data or an object of class 'Matrix', 'data.table', 'data.frame', 'matrix' 'DelayedMatrix' or 'dbSparseMatrix'."
- **Root cause:** The `createExprObj()` function is not recognizing the input as a valid dbMatrix object type. This suggests either a class definition issue or a missing implementation for dbMatrix objects.
- **Proposed fix:** Ensure that the `dbSparseMatrix` class is properly defined and recognized in the `.evaluate_expr_matrix` function.

#### Issue 3: SQL ORDER BY warnings
- **Warning type:** "ORDER BY is ignored in subqueries without LIMIT"
- **Root cause:** When constructing SQL queries with dbplyr, the code is using `arrange()` in subqueries without LIMIT clauses, which is ineffective.
- **Proposed fix:** Modify the SQL query generation in dbMatrix functions to either:
  1. Move `arrange()` calls later in the pipeline
  2. Replace with `window_order()` as suggested in the warning
  3. Add a LIMIT clause where appropriate

### Next Steps
- Inspect the source code for subsetting operations in GiottoClass for dbMatrix
- Review how dbMatrix objects are registered and handled in the Giotto expression evaluation pipeline
- Check implementation of the processExpression functions for dbMatrix to ensure they respect memory contracts

## Final Results

- [ ] All tests pass
- [ ] Some tests still failing (see issues section)